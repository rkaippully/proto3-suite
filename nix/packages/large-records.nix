{ mkDerivation, base, containers, fetchgit, generic-deriving, ghc
, large-generics, lib, mtl, newtype, primitive
, record-dot-preprocessor, record-hasfield, syb, tasty, tasty-hunit
, template-haskell, transformers
}:
mkDerivation {
  pname = "large-records";
  version = "0.3";
  src = fetchgit {
    url = "https://github.com/rkaippully/large-records.git";
    sha256 = "sha256-ZyUnG02s0boqaWXJRI2SWojdkEbHZzR9/StmfbysSRc=";
    rev = "b47fd98cf306cbf1292dfd438180215f213ccf4e";
  };
  postUnpack = "sourceRoot+=/large-records; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base containers ghc large-generics mtl primitive record-hasfield
    syb template-haskell transformers
  ];
  testHaskellDepends = [
    base generic-deriving large-generics mtl newtype
    record-dot-preprocessor record-hasfield tasty tasty-hunit
    template-haskell transformers
  ];
  description = "Efficient compilation for large records, linear in the size of the record";
  license = lib.licenses.bsd3;
}
