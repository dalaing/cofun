{ mkDerivation, base, cofun-console, cofun-pairing, comonad, free
, mtl, parsec, parsers, stdenv, transformers
}:
mkDerivation {
  pname = "adder";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [
    base cofun-console cofun-pairing comonad free mtl parsec parsers
    transformers
  ];
  homepage = "http://dlaing.org/cofun";
  license = stdenv.lib.licenses.bsd3;
}
