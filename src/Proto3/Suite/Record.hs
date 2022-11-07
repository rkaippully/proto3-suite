{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DefaultSignatures #-}

module Proto3.Suite.Record
  ( -- * large-records support
    LRGenericMessage (..)
  , MessageFieldMeta (..)
  , OneOfMessage(..)
  , OneOfFromJSONPB(..)
  , OneOfToKeyValuePB(..)
  , toDotProtoFields
  , lrGenericDef
  , lrGenericEncodeMessage
  , lrGenericDecodeMessage

  , lrGenericParseJSONPB
  , lrGenericToJSONPB
  , lrGenericToEncodingPB
  , lrGenericDeclareNamedSchema
  ) where

import           Control.Applicative
import           Data.List                       (stripPrefix)
import           Proto3.Suite.DotProto.Internal  (toLowerFirst, toUpperFirst)
import           Proto3.Wire.Decode              (Parser (..), RawMessage)
import qualified Proto3.Wire.Encode              as Encode
import qualified Proto3.Wire.Decode              as Decode
import qualified Data.Vector                     as V
import           Proto3.Suite.DotProto
import           Proto3.Suite.Types
import           Proto3.Suite.Class
import           Proto3.Suite.JSONPB.Class
import           GHC.Exts                        (Proxy#, proxy#)
import           Data.Proxy                      (Proxy (..))
import qualified Data.Text                       as T
import qualified Data.Aeson                      as A
import qualified Data.Aeson.Types                as A
import           GHC.Generics
import           Data.Record.Generic             as LG ((:.:)(Comp), I (..), K (..))
import qualified Data.Record.Generic             as LG
import qualified Data.Record.Generic.Rep         as LG
import qualified Data.Record.Plugin.Runtime      as LR
#ifdef SWAGGER
import           Data.Swagger                           (Definitions,
                                                         NamedSchema (..),
                                                         ParamSchema (..),
                                                         Referenced,
                                                         Schema (..),
                                                         SwaggerType (SwaggerObject),
                                                         ToSchema (..),
                                                         declareSchemaRef)
import           Data.Swagger.Declare                   (Declare)
import           Proto3.Suite.DotProto.Generate.Swagger (OverrideToSchema (..),
                                                         asProxy,
                                                         insOrdFromList)
#endif


-- | A generic 'def' implementation for large-record types.
lrGenericDef :: (LR.Generic a, LR.Constraints a HasDefault) => a
lrGenericDef = LR.to $ LG.cpure (Proxy @HasDefault) (pure def)

data MessageFieldMeta where
  MessageFieldMeta ::
    (MessageField w, HasDefault f, FromJSONPB f, ToJSONPB f, ToSchema (OverrideToSchema f)) =>
    Proxy f -> Proxy w -> DotProtoField -> MessageFieldMeta
  MessageNestedFieldMeta ::
    (MessageField (Nested w), FromJSONPB (Maybe f), ToJSONPB (Maybe f), ToSchema (OverrideToSchema (Maybe f))) =>
    Proxy (Maybe f) -> Proxy (Nested w) -> DotProtoField -> MessageFieldMeta
  MessageNestedVecFieldMeta ::
    (MessageField (NestedVec w), FromJSONPB (V.Vector f), ToJSONPB (V.Vector f), ToSchema (OverrideToSchema (V.Vector f))) =>
    Proxy (V.Vector f) -> Proxy (NestedVec w) -> DotProtoField -> MessageFieldMeta
  MessagePackedVecFieldMeta ::
    (MessageField (PackedVec w), FromJSONPB (V.Vector f), ToJSONPB (V.Vector f), ToSchema (OverrideToSchema (V.Vector f))) =>
    Proxy (V.Vector f) -> Proxy (PackedVec w) -> DotProtoField -> MessageFieldMeta
  MessageUnpackedVecFieldMeta ::
    (MessageField (UnpackedVec w), FromJSONPB (V.Vector f), ToJSONPB (V.Vector f), ToSchema (OverrideToSchema (V.Vector f))) =>
    Proxy (V.Vector f) -> Proxy (UnpackedVec w) -> DotProtoField -> MessageFieldMeta
  MessageOneOfFieldMeta ::
    (OneOfMessage f, OneOfFromJSONPB f, OneOfToKeyValuePB f, ToSchema (OverrideToSchema (Maybe f))) =>
    Proxy (Maybe f) -> String -> MessageFieldMeta

deriving stock instance Show MessageFieldMeta


class LR.Generic a => LRGenericMessage a where
  lrGenericDotProto :: Proxy# a -> LR.Rep (K MessageFieldMeta) a

toDotProtoFields :: LRGenericMessage a => Proxy# a -> [DotProtoField]
toDotProtoFields x = foldMap aux $ LG.collapse $ lrGenericDotProto x
  where
   aux (MessageFieldMeta _ _ fld) = [fld]
   aux (MessageNestedFieldMeta _ _ fld) = [fld]
   aux (MessageNestedVecFieldMeta _ _ fld) = [fld]
   aux (MessagePackedVecFieldMeta _ _ fld) = [fld]
   aux (MessageUnpackedVecFieldMeta _ _ fld) = [fld]
   aux (MessageOneOfFieldMeta _ _) = []

class OneOfMessage a where
  encodeOneOfMessage :: Maybe a -> Encode.MessageBuilder
  decodeOneOfMessage :: Parser RawMessage (Maybe a)

lrEncodeMessageField :: forall x. MessageFieldMeta -> x -> Encode.MessageBuilder
lrEncodeMessageField meta x =
  case meta of
    MessageFieldMeta _ (Proxy :: Proxy w) DotProtoField{dotProtoFieldNumber} ->
      encodeMessageField dotProtoFieldNumber (LR.unsafeCoerce @x @w x)
    MessageNestedFieldMeta _ (Proxy :: Proxy (Nested w)) DotProtoField{dotProtoFieldNumber} ->
      encodeMessageField dotProtoFieldNumber (LR.unsafeCoerce @x @(Nested w) x)
    MessageNestedVecFieldMeta _ (Proxy :: Proxy (NestedVec w)) DotProtoField{dotProtoFieldNumber} ->
      encodeMessageField dotProtoFieldNumber (LR.unsafeCoerce @x @(NestedVec w) x)
    MessagePackedVecFieldMeta _ (Proxy :: Proxy (PackedVec w)) DotProtoField{dotProtoFieldNumber} ->
      encodeMessageField dotProtoFieldNumber (LR.unsafeCoerce @x @(PackedVec w) x)
    MessageUnpackedVecFieldMeta _ (Proxy :: Proxy (UnpackedVec w)) DotProtoField{dotProtoFieldNumber} ->
      encodeMessageField dotProtoFieldNumber (LR.unsafeCoerce @x @(UnpackedVec w) x)
    MessageOneOfFieldMeta (Proxy :: Proxy (Maybe f)) _ ->
      encodeOneOfMessage (LR.unsafeCoerce @x @(Maybe f) x)
    _ -> mempty

lrGenericEncodeMessage :: forall a. LRGenericMessage a => a -> Encode.MessageBuilder
lrGenericEncodeMessage =
  mconcat
  . LG.collapse
  . LG.zipWith (LG.mapKIK lrEncodeMessageField)
               (lrGenericDotProto (proxy# :: Proxy# a))
  . LG.from

lrDecodeMessageField :: forall x. MessageFieldMeta -> (Parser RawMessage LG.:.: I) x
lrDecodeMessageField meta =
  case meta of
    MessageFieldMeta _ (Proxy :: Proxy w) DotProtoField{dotProtoFieldNumber} ->
      Comp $ I <$> LR.unsafeCoerce @(_ w) @(_ x) (Decode.at decodeMessageField dotProtoFieldNumber)
    MessageNestedFieldMeta _ (Proxy :: Proxy (Nested w)) DotProtoField{dotProtoFieldNumber} ->
      Comp $ I <$> LR.unsafeCoerce @(_ (Nested w)) @(_ x) (Decode.at decodeMessageField dotProtoFieldNumber)
    MessageNestedVecFieldMeta _ (Proxy :: Proxy (NestedVec w)) DotProtoField{dotProtoFieldNumber} ->
      Comp $ I <$> LR.unsafeCoerce @(_ (NestedVec w)) @(_ x) (Decode.at decodeMessageField dotProtoFieldNumber)
    MessagePackedVecFieldMeta _ (Proxy :: Proxy (PackedVec w)) DotProtoField{dotProtoFieldNumber} ->
      Comp $ I <$> LR.unsafeCoerce @(_ (PackedVec w)) @(_ x) (Decode.at decodeMessageField dotProtoFieldNumber)
    MessageUnpackedVecFieldMeta _ (Proxy :: Proxy (UnpackedVec w)) DotProtoField{dotProtoFieldNumber} ->
      Comp $ I <$> LR.unsafeCoerce @(_ (UnpackedVec w)) @(_ x) (Decode.at decodeMessageField dotProtoFieldNumber)
    MessageOneOfFieldMeta (Proxy :: Proxy (Maybe f)) _ ->
      Comp $ I <$> LR.unsafeCoerce @(_ (Maybe f)) @(_ x) decodeOneOfMessage
    _ ->
      Comp $ Parser $ const $ Left $ Decode.WireTypeError "Invalid generic representation"

lrGenericDecodeMessage :: forall a. LRGenericMessage a => Parser RawMessage a
lrGenericDecodeMessage = LG.to <$> LG.sequenceA decoders
  where
    decoders = LG.map (lrDecodeMessageField . LG.unK)
                      (lrGenericDotProto (proxy# :: Proxy# a))


--------------------------------------------------------------------------------
-- Instances for JSONPB

class OneOfFromJSONPB a where
  oneOfFromJSONPB :: String -> String -> A.Object -> A.Parser a

  default oneOfFromJSONPB :: (Generic a, GOneOfFromJSONPB (Rep a)) => String -> String -> A.Object -> A.Parser a
  oneOfFromJSONPB messageName fieldName obj =
    GHC.Generics.to <$> gOneOfFromJSONPB messageName fieldName obj

instance OneOfFromJSONPB a => OneOfFromJSONPB (Maybe a) where
  oneOfFromJSONPB :: String -> String -> A.Object -> A.Parser (Maybe a)
  oneOfFromJSONPB messageName fieldName obj =
    fmap Just (oneOfFromJSONPB messageName fieldName obj)
    <|> pure Nothing

parseOneOfJSONPB :: OneOfFromJSONPB a => String -> String -> A.Value -> A.Parser a
parseOneOfJSONPB messageName fieldName =
  A.withObject messageName $ \obj ->
    parseNamedOneOf obj <|> parseBareOneOf obj
  where
    parseNamedOneOf obj = (obj .: T.pack fieldName) >>=
      A.withObject fieldName (oneOfFromJSONPB messageName fieldName)

    parseBareOneOf obj = oneOfFromJSONPB messageName fieldName obj


class GOneOfFromJSONPB f where
  gOneOfFromJSONPB :: String -> String -> A.Object -> A.Parser (f p)

instance GOneOfFromJSONPB f => GOneOfFromJSONPB (M1 D c f) where
  gOneOfFromJSONPB messageName fieldName obj = M1 <$> gOneOfFromJSONPB messageName fieldName obj

instance (Constructor c, GOneOfFromJSONPB f) => GOneOfFromJSONPB (M1 C c f) where
  gOneOfFromJSONPB messageName fieldName obj =
    case stripPrefix prefix name of
      Nothing -> fail $ "Constructor name " ++ name ++ " does not start with " ++ prefix
      Just fld -> M1 <$> gOneOfFromJSONPB messageName (toLowerFirst fld) obj
    where
      name = conName (undefined :: M1 C c f ())
      prefix = messageName ++ toUpperFirst fieldName

instance (Selector s, GOneOfFromJSONPB f) => GOneOfFromJSONPB (M1 S s f) where
  gOneOfFromJSONPB messageName fieldName obj = M1 <$> gOneOfFromJSONPB messageName fieldName obj

instance FromJSONPB a => GOneOfFromJSONPB (K1 R a) where
  gOneOfFromJSONPB _messageName fieldName obj = K1 <$> parseField obj (T.pack fieldName)

instance (GOneOfFromJSONPB f, GOneOfFromJSONPB g) => GOneOfFromJSONPB (f :+: g) where
  gOneOfFromJSONPB messageName fieldName obj =
    (L1 <$> gOneOfFromJSONPB messageName fieldName obj) <|>
    (R1 <$> gOneOfFromJSONPB messageName fieldName obj)


lrGenericParseJSONPB :: forall a. (Named a, LRGenericMessage a) => A.Value -> A.Parser a
lrGenericParseJSONPB =
  let
    messageName = nameOf (proxy# :: Proxy# a)
  in
    A.withObject messageName $ \obj ->
      let aux :: forall x. MessageFieldMeta -> (A.Parser LG.:.: I) x
          aux (MessageFieldMeta (Proxy :: Proxy f) _ DotProtoField{dotProtoFieldName = Single fld}) =
            LG.Comp $ I <$> LR.unsafeCoerce @(_ f) @(_ x) (obj .: T.pack fld)
          aux (MessageNestedFieldMeta (Proxy :: Proxy (Maybe f)) _ DotProtoField{dotProtoFieldName = Single fld}) =
            LG.Comp $ I <$> LR.unsafeCoerce @(_ (Maybe f)) @(_ x) (obj .: T.pack fld)
          aux (MessageNestedVecFieldMeta (Proxy :: Proxy (V.Vector f)) _ DotProtoField{dotProtoFieldName = Single fld}) =
            LG.Comp $ I <$> LR.unsafeCoerce @(_ (V.Vector f)) @(_ x) (obj .: T.pack fld)
          aux (MessagePackedVecFieldMeta (Proxy :: Proxy (V.Vector f)) _ DotProtoField{dotProtoFieldName = Single fld}) =
            LG.Comp $ I <$> LR.unsafeCoerce @(_ (V.Vector f)) @(_ x) (obj .: T.pack fld)
          aux (MessageUnpackedVecFieldMeta (Proxy :: Proxy (V.Vector f)) _ DotProtoField{dotProtoFieldName = Single fld}) =
            LG.Comp $ I <$> LR.unsafeCoerce @(_ (V.Vector f)) @(_ x) (obj .: T.pack fld)
          aux (MessageOneOfFieldMeta (Proxy :: Proxy f) fieldName) =
            LG.Comp $ I <$> LR.unsafeCoerce @(_ f) @(_ x) (parseOneOfJSONPB messageName fieldName $ A.Object obj)
          aux meta = LG.Comp $ fail $ "Invalid field: " <> show meta
      in LG.to <$> LG.sequenceA (LG.map (aux . LG.unK) (lrGenericDotProto (proxy# :: Proxy# a)))


class OneOfToKeyValuePB a where
  oneOfToKeyValuePB :: KeyValuePB kv => String -> String -> a -> Options -> kv

  default oneOfToKeyValuePB :: (Generic a, GOneOfToKeyValuePB (Rep a), KeyValuePB kv) => String -> String -> a -> Options -> kv
  oneOfToKeyValuePB messageName fieldName x = gOneOfToKeyValuePB messageName fieldName (GHC.Generics.from x)

instance OneOfToKeyValuePB a => OneOfToKeyValuePB (Maybe a) where
  oneOfToKeyValuePB :: KeyValuePB kv => String -> String -> Maybe a -> Options -> kv
  oneOfToKeyValuePB messageName fieldName = maybe (const mempty) (oneOfToKeyValuePB messageName fieldName)

class GOneOfToKeyValuePB f where
  gOneOfToKeyValuePB :: KeyValuePB kv => String -> String -> f p -> Options -> kv

instance GOneOfToKeyValuePB f => GOneOfToKeyValuePB (M1 D c f) where
  gOneOfToKeyValuePB messageName fieldName (M1 x) = gOneOfToKeyValuePB messageName fieldName x

instance (Constructor c, GOneOfToKeyValuePB f) => GOneOfToKeyValuePB (M1 C c f) where
  gOneOfToKeyValuePB messageName fieldName (M1 x) =
    case stripPrefix prefix name of
      Nothing -> const mempty
      Just fld -> gOneOfToKeyValuePB messageName (toLowerFirst fld) x
    where
      name = conName (undefined :: M1 C c f ())
      prefix = messageName ++ toUpperFirst fieldName

instance (Selector s, GOneOfToKeyValuePB f) => GOneOfToKeyValuePB (M1 S s f) where
  gOneOfToKeyValuePB messageName fieldName (M1 x) = gOneOfToKeyValuePB messageName fieldName x

instance (HasDefault a, ToJSONPB a) => GOneOfToKeyValuePB (K1 R a) where
  gOneOfToKeyValuePB _messageName fieldName (K1 x) = pair (T.pack fieldName) x

instance (GOneOfToKeyValuePB f, GOneOfToKeyValuePB g) => GOneOfToKeyValuePB (f :+: g) where
  gOneOfToKeyValuePB messageName fieldName = \case
    L1 l -> gOneOfToKeyValuePB messageName fieldName l
    R1 r -> gOneOfToKeyValuePB messageName fieldName r


lrGenericToJSONPB :: forall a. (Named a, LRGenericMessage a) => a -> Options -> A.Value
lrGenericToJSONPB x options =
  let
    messageName = nameOf (proxy# :: Proxy# a)
    ps :: [Options -> [A.Pair]]
    ps = LG.collapse
         $ LG.zipWith (LG.mapKIK (toJSONPairs messageName))
                      (lrGenericDotProto (proxy# :: Proxy# a))
         $ LG.from x
  in
    object ps options

lrGenericToEncodingPB :: forall a. (Named a, LRGenericMessage a) => a -> Options -> A.Encoding
lrGenericToEncodingPB x options =
  let
    messageName = nameOf (proxy# :: Proxy# a)
    ps :: [Options -> A.Series]
    ps = LG.collapse
         $ LG.zipWith (LG.mapKIK (toJSONPairs messageName))
                      (lrGenericDotProto (proxy# :: Proxy# a))
         $ LG.from x
  in
    pairs ps options

toJSONPairs :: forall x kv. KeyValuePB kv => String -> MessageFieldMeta -> x -> (Options -> kv)
toJSONPairs messageName meta x =
  case meta of
    MessageFieldMeta (Proxy :: Proxy f) _ DotProtoField{dotProtoFieldName = Single fld} ->
      T.pack fld .= LR.unsafeCoerce @x @f x
    MessageNestedFieldMeta (Proxy :: Proxy (Maybe f)) _ DotProtoField{dotProtoFieldName = Single fld} ->
      T.pack fld .= LR.unsafeCoerce @x @(Maybe f) x
    MessageNestedVecFieldMeta (Proxy :: Proxy (V.Vector f)) _ DotProtoField{dotProtoFieldName = Single fld} ->
      T.pack fld .= LR.unsafeCoerce @x @(V.Vector f) x
    MessagePackedVecFieldMeta (Proxy :: Proxy (V.Vector f)) _ DotProtoField{dotProtoFieldName = Single fld} ->
      T.pack fld .= LR.unsafeCoerce @x @(V.Vector f) x
    MessageUnpackedVecFieldMeta (Proxy :: Proxy (V.Vector f)) _ DotProtoField{dotProtoFieldName = Single fld} ->
      T.pack fld .= LR.unsafeCoerce @x @(V.Vector f) x
    MessageOneOfFieldMeta (Proxy :: Proxy f) fld -> \options ->
      if optEmitNamedOneof options then
        (T.pack fld .= objectOrNull [oneOfToKeyValuePB messageName fld (LR.unsafeCoerce @x @f x)] options)
          options
      else
        oneOfToKeyValuePB messageName fld (LR.unsafeCoerce @x @f x) options
    _ -> const mempty


#ifdef SWAGGER

lrGenericDeclareNamedSchema :: forall a. (Named a, LRGenericMessage a) => Proxy a -> Declare (Definitions Schema) NamedSchema
lrGenericDeclareNamedSchema _ = do
  let fields = lrGenericDotProto (proxy# :: Proxy# a)

  let declareFields :: forall x. MessageFieldMeta -> (Proxy LG.:.: I) x
      declareFields = \case
        MessageFieldMeta (Proxy :: Proxy f) _ _ ->
          LG.Comp $ I <$> LR.unsafeCoerce @(_ f) @(_ x) (asProxy $ declareSchemaRef @(OverrideToSchema f))
        MessageNestedFieldMeta (Proxy :: Proxy (Maybe f)) _ _ ->
          LG.Comp $ I <$> LR.unsafeCoerce @(_ (Maybe f)) @(_ x) (asProxy $ declareSchemaRef @(OverrideToSchema (Maybe f)))
        MessageNestedVecFieldMeta (Proxy :: Proxy (V.Vector f)) _ _ ->
          LG.Comp $ I <$> LR.unsafeCoerce @(_ (V.Vector f)) @(_ x) (asProxy $ declareSchemaRef @(OverrideToSchema (V.Vector f)))
        MessagePackedVecFieldMeta (Proxy :: Proxy (V.Vector f)) _ _ ->
          LG.Comp $ I <$> LR.unsafeCoerce @(_ (V.Vector f)) @(_ x) (asProxy $ declareSchemaRef @(OverrideToSchema (V.Vector f)))
        MessageUnpackedVecFieldMeta (Proxy :: Proxy (V.Vector f)) _ _ ->
          LG.Comp $ I <$> LR.unsafeCoerce @(_ (V.Vector f)) @(_ x) (asProxy $ declareSchemaRef @(OverrideToSchema (V.Vector f)))
        MessageOneOfFieldMeta (Proxy :: Proxy (Maybe f)) _ ->
          LG.Comp $ I <$> LR.unsafeCoerce @(_ (Maybe f)) @(_ x) (asProxy $ declareSchemaRef @(OverrideToSchema (Maybe f)))
  let _p :: Proxy a = LG.to <$> LG.sequenceA (LG.map (declareFields . LG.unK) fields)

  let makeSchemaField :: forall x. MessageFieldMeta -> (Declare (Definitions Schema) LG.:.: K [(T.Text, Referenced Schema)]) x
      makeSchemaField = \case
        MessageFieldMeta (Proxy :: Proxy f) _ DotProtoField{dotProtoFieldName = Single fld} ->
          LG.Comp $ do
            schema <- declareSchemaRef (Proxy @(OverrideToSchema f))
            pure $ K $ [(T.pack fld, schema)]
        MessageNestedFieldMeta (Proxy :: Proxy (Maybe f)) _ DotProtoField{dotProtoFieldName = Single fld} ->
          LG.Comp $ do
            schema <- declareSchemaRef (Proxy @(OverrideToSchema (Maybe f)))
            pure $ K $ [(T.pack fld, schema)]
        MessageNestedVecFieldMeta (Proxy :: Proxy (V.Vector f)) _ DotProtoField{dotProtoFieldName = Single fld} ->
          LG.Comp $ do
            schema <- declareSchemaRef (Proxy @(OverrideToSchema (V.Vector f)))
            pure $ K $ [(T.pack fld, schema)]
        MessagePackedVecFieldMeta (Proxy :: Proxy (V.Vector f)) _ DotProtoField{dotProtoFieldName = Single fld} ->
          LG.Comp $ do
            schema <- declareSchemaRef (Proxy @(OverrideToSchema (V.Vector f)))
            pure $ K $ [(T.pack fld, schema)]
        MessageUnpackedVecFieldMeta (Proxy :: Proxy (V.Vector f)) _ DotProtoField{dotProtoFieldName = Single fld} ->
          LG.Comp $ do
            schema <- declareSchemaRef (Proxy @(OverrideToSchema (V.Vector f)))
            pure $ K $ [(T.pack fld, schema)]
        MessageOneOfFieldMeta (Proxy :: Proxy (Maybe f)) fld ->
          LG.Comp $ do
            schema <- declareSchemaRef (Proxy @(OverrideToSchema (Maybe f)))
            pure $ K $ [(T.pack fld, schema)]
        _ -> LG.Comp $ pure $ K []

  schemaFields <- LG.sequenceA $ LG.map (makeSchemaField . LG.unK) fields

  Prelude.pure
    NamedSchema
      { _namedSchemaName = Just (nameOf (proxy# :: Proxy# a))
      , _namedSchemaSchema =
          mempty
            { _schemaParamSchema = mempty {
#if MIN_VERSION_swagger2(2,4,0)
                                            _paramSchemaType = Just SwaggerObject
#else
                                            _paramSchemaType = SwaggerObject
#endif
                                          }
            , _schemaProperties = insOrdFromList $ concat (LG.collapse schemaFields)
            }
      }
#endif
