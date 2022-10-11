{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Proto3.Suite.DhallPB
  ( -- * Modules
    module Dhall

    -- * large-record support
  , lrGenericInjectWith
  , lrGenericAutoWith
  ) where

import qualified Data.ByteString
import qualified Data.ByteString.Base64
import qualified Data.ByteString.Base64.Lazy
import qualified Data.ByteString.Lazy
import Data.Functor.Contravariant (contramap)
import Data.Int (Int32, Int64)
import Data.Proxy (Proxy (..))
import qualified Data.Record.Generic as LG
import qualified Data.Record.Generic.Rep as LG
import Data.SOP.BasicFunctors ((:.:) (..), I (..), K (..))
import qualified Data.Text as Text
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy.Encoding
import Data.Void (Void)
import Data.Word (Word32, Word64)
import Dhall (FromDhall (..), ToDhall (..))
import qualified Dhall
import qualified Dhall.Core as Dhall
import qualified Dhall.Map as Dhall
import qualified Dhall.Src as Dhall
import GHC.Float (double2Float, float2Double)
import Proto3.Suite.Types (Enumerated (..), Fixed (..))

--------------------------------------------------------------------------------
-- Interpret the special 'Enumerated' type

instance Dhall.FromDhall a => Dhall.FromDhall (Enumerated a)

instance Dhall.FromDhall a => Dhall.FromDhall (Either Int32 a)

--------------------------------------------------------------------------------
-- Interpret the strict and lazy ByteString types
--
-- We must base-64 decode a 'ByteString' after encoding it from a Text
-- because it may contain invalid UTF-8 data and Dhall does not have a
-- native type for bytes.

instance Dhall.FromDhall Data.ByteString.Lazy.ByteString where
  autoWith _ = fmap b64Decode Dhall.lazyText
    where
      b64Decode = Data.ByteString.Base64.Lazy.decodeLenient . Data.Text.Lazy.Encoding.encodeUtf8

instance Dhall.FromDhall Data.ByteString.ByteString where
  autoWith _ = fmap b64Decode Dhall.strictText
    where
      b64Decode =  Data.ByteString.Base64.decodeLenient . Data.Text.Encoding.encodeUtf8

--------------------------------------------------------------------------------
-- Interpret integer scalar types

-- Lossy interpretation and integer overflow can happen with the
-- following instances because the 'Dhall.Integer' (Dhall's only
-- integer type) equals Haskell's Integer type. We don't expect these
-- instances to introduce integer overflow because they should only
-- interpret Dhall rendered from protobuf messages created with
-- generated code.
--
-- TODO: we should perform run-time bounds-checking to at least hint
-- to the user that we interpreted something bad.

#if !MIN_VERSION_dhall(1,35,0)
instance Dhall.FromDhall Int where
  autoWith _ = fmap fromInteger Dhall.integer

instance Dhall.FromDhall Int32 where
  autoWith _ = fmap fromInteger Dhall.integer

instance Dhall.FromDhall Int64 where
  autoWith _ = fmap fromInteger Dhall.integer

instance Dhall.FromDhall Word32 where
  autoWith _ = fmap fromIntegral Dhall.integer

instance Dhall.FromDhall Word64 where
  autoWith _ = fmap fromIntegral Dhall.integer
#endif

instance Dhall.FromDhall (Fixed Int32) where
  autoWith = fmap Fixed . Dhall.autoWith

instance Dhall.FromDhall (Fixed Int64) where
  autoWith = fmap Fixed . Dhall.autoWith

instance Dhall.FromDhall (Fixed Word32) where
  autoWith = fmap Fixed . Dhall.autoWith

instance Dhall.FromDhall (Fixed Word64) where
  autoWith = fmap Fixed . Dhall.autoWith

--------------------------------------------------------------------------------
-- Interpret floating point scalar types
--
-- Loss of precision can happen when converting a 'Double' to a
-- 'Float'. We don't expect this instance to introduce loss of
-- precision because it should only interpret Dhall rendered from
-- protobuf messages created with generated code. The Dhall rendering
-- converts from a 'Float' to the 'Dhall.Double' type.

instance Dhall.FromDhall Float where
  autoWith _ = fmap double2Float Dhall.double

--------------------------------------------------------------------------------
-- Inject the special 'Enumerated' type

instance Dhall.ToDhall a => Dhall.ToDhall (Enumerated a)

instance Dhall.ToDhall a => Dhall.ToDhall (Either Int32 a)

--------------------------------------------------------------------------------
-- Inject integer scalar types

instance Dhall.ToDhall Int32 where
  injectWith = fmap (contramap toInteger) Dhall.injectWith

instance Dhall.ToDhall Int64 where
  injectWith = fmap (contramap toInteger) Dhall.injectWith

instance Dhall.ToDhall (Fixed Int32) where
  injectWith = fmap (contramap fixed) Dhall.injectWith

instance Dhall.ToDhall (Fixed Int64) where
  injectWith = fmap (contramap fixed) Dhall.injectWith

instance Dhall.ToDhall (Fixed Word32) where
  injectWith = fmap (contramap fixed) Dhall.injectWith

instance Dhall.ToDhall (Fixed Word64) where
  injectWith = fmap (contramap fixed) Dhall.injectWith

--------------------------------------------------------------------------------
-- Inject floating point scalar types

instance Dhall.ToDhall Float where
  injectWith = fmap (contramap float2Double) Dhall.injectWith

--------------------------------------------------------------------------------
-- Inject strict and lazy ByteStrings
--
-- We must base-64 encode a 'ByteString' before decoding it to a Text
-- because it may contain invalid UTF-8 data and Dhall does not have a
-- native type for bytes.

instance Dhall.ToDhall Data.ByteString.Lazy.ByteString where
  injectWith = fmap (contramap b64Encode) Dhall.injectWith
    where
      -- 'decodeUtf8' will throw an error on any invalid UTF-8 data
      -- but we should never encounter that case with this usage
      -- because we Base64 encode the ByteString first
      b64Encode = Data.Text.Lazy.Encoding.decodeUtf8 . Data.ByteString.Base64.Lazy.encode

instance Dhall.ToDhall Data.ByteString.ByteString where
  injectWith = fmap (contramap b64Encode) Dhall.injectWith
    where
      -- 'decodeUtf8' will throw an error on any invalid UTF-8 data
      -- but we should never encounter that case with this usage
      -- because we Base64 encode the ByteString first
      b64Encode = Data.Text.Encoding.decodeUtf8 . Data.ByteString.Base64.encode

{- Generic deriving of classes for large-records

 The internal representation of the types defined using the large-record package
 is not compatible with GHC Generics. The functions below provide large-record
 compatible implementations of methods for some type classes.
-}

{-| A generic 'autoWith' implementation for large-record types. -}
lrGenericAutoWith :: forall a. (LG.Generic a, LG.Constraints a Dhall.FromDhall) => Dhall.InputNormalizer -> Dhall.Decoder a
lrGenericAutoWith normalizer = Dhall.Decoder extract expected
  where
    makeCompRep ::
      (forall x. Dhall.FromDhall x => K String x -> f (g x)) ->
      LG.Rep (f :.: g) a
    makeCompRep f = LG.cmap
                      (Proxy @Dhall.FromDhall)
                      (Comp . f)
                      (LG.recordFieldNames $ LG.metadata (Proxy @a))

    extract :: Dhall.Expr Dhall.Src Void -> Dhall.Extractor Dhall.Src Void a
    extract expr =
      case expr of
        Dhall.RecordLit flds -> do
          let getField :: forall x. Dhall.FromDhall x => K String x -> Dhall.Extractor Dhall.Src Void x
              getField (K fld) =
                let decoder = Dhall.autoWith @x normalizer
                in maybe
                     (Dhall.typeError expected expr)
                     (Dhall.extract decoder)
#if MIN_VERSION_dhall(1,34,0)
                     (Dhall.recordFieldValue <$> Dhall.lookup (Text.pack fld) flds)
#else
                     (Dhall.lookup (Text.pack fld) flds)
#endif
          LG.to <$> LG.sequenceA (makeCompRep (fmap I . getField))
        _ -> Dhall.typeError expected expr

#if MIN_VERSION_dhall(1,34,0)
    expected :: Dhall.Expector (Dhall.Expr Dhall.Src Void)
    expected = do
      let getField :: forall x. Dhall.FromDhall x => K String x -> Dhall.Expector (Dhall.Text, Dhall.RecordField Dhall.Src Void)
          getField (K fld) = do
            let decoder = Dhall.autoWith @x normalizer
            f <- Dhall.makeRecordField <$> Dhall.expected decoder
            pure (Text.pack fld, f)
#elif MIN_VERSION_dhall(1,33,0)
    expected :: Dhall.Expector (Dhall.Expr Dhall.Src Void)
    expected = do
      let getField :: forall x. Dhall.FromDhall x => K String x -> Dhall.Expector (Dhall.Text, Dhall.Expr Dhall.Src Void)
          getField (K fld) = do
            let decoder = Dhall.autoWith @x normalizer
            f <- Dhall.expected decoder
            pure (Text.pack fld, f)
#else
    expected :: Dhall.Expr Dhall.Src Void
    expected = LG.unI $ do
      let getField :: forall x. Dhall.FromDhall x => K String x -> I (Dhall.Text, Dhall.Expr Dhall.Src Void)
          getField (K fld) = do
            let decoder = Dhall.autoWith @x normalizer
            let f = Dhall.expected decoder
            pure (Text.pack fld, f)
#endif
      Dhall.Record . Dhall.fromList . LG.collapse <$> LG.sequenceA (makeCompRep (fmap K . getField))

{-| A generic 'injectWith' implementation for large-record types. -}
lrGenericInjectWith :: forall a. (LG.Generic a, LG.Constraints a Dhall.ToDhall) => Dhall.InputNormalizer -> Dhall.Encoder a
lrGenericInjectWith normalizer = Dhall.Encoder embed declared
  where
    md = LG.metadata (Proxy @a)
    fieldNames = LG.recordFieldNames md

    embed :: a -> Dhall.Expr Dhall.Src Void
    embed = Dhall.RecordLit
              . Dhall.fromList
              . LG.collapse
#if MIN_VERSION_dhall(1,34,0)
              . LG.zipWith (LG.mapKKK $ \n x -> (Text.pack n, Dhall.makeRecordField x)) fieldNames
#else
              . LG.zipWith (LG.mapKKK $ \n x -> (Text.pack n, x)) fieldNames
#endif
              . LG.cmap (Proxy @Dhall.ToDhall) (K . Dhall.embed (injectWith normalizer) . LG.unI)
              . LG.from

    declared :: Dhall.Expr Dhall.Src Void
    declared = Dhall.Record
                 $ Dhall.fromList
                 $ LG.collapse
                 $ LG.cmap
                     (Proxy @Dhall.ToDhall)
                     (\(K n :: K String x) ->
                          let typ = Dhall.declared (injectWith @x normalizer)
#if MIN_VERSION_dhall(1,34,0)
                          in K (Text.pack n, Dhall.makeRecordField typ)
#else
                          in K (Text.pack n, typ)
#endif
                     )
                     fieldNames
