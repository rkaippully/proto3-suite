{ mkDerivation, aeson, base, fetchgit, generic-deriving, generics-sop
, lib, microlens, mtl, primitive, QuickCheck, sop-core, tasty
, tasty-hunit, tasty-quickcheck
}:
mkDerivation {
  pname = "large-generics";
  version = "0.2.0.0";
  src = fetchgit {
    url = "https://github.com/rkaippully/large-records.git";
    sha256 = "sha256-ZyUnG02s0boqaWXJRI2SWojdkEbHZzR9/StmfbysSRc=";
    rev = "b47fd98cf306cbf1292dfd438180215f213ccf4e";
  };
  postUnpack = "sourceRoot+=/large-generics; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson base generics-sop primitive sop-core
  ];
  testHaskellDepends = [
    aeson base generic-deriving generics-sop microlens mtl QuickCheck
    sop-core tasty tasty-hunit tasty-quickcheck
  ];
  description = "Generic programming API for large-records and large-anon";
  license = lib.licenses.bsd3;
}
