{ mkDerivation, base, comonad, containers, QuickCheck, stdenv
, tasty, tasty-quickcheck
}:
mkDerivation {
  pname = "cofun-zipper";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [
    base comonad containers QuickCheck tasty tasty-quickcheck
  ];
  testDepends = [
    base comonad containers QuickCheck tasty tasty-quickcheck
  ];
  homepage = "htp://dlaing.org/cofun";
  license = stdenv.lib.licenses.bsd3;
}
