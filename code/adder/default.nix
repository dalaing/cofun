{ mkDerivation, base, binary, cofun-console, cofun-network
, cofun-pairing, comonad, free, mtl, network-simple, parsec
, parsers, stdenv, transformers
}:
mkDerivation {
  pname = "adder";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base binary cofun-console cofun-network cofun-pairing comonad free
    mtl network-simple parsec parsers transformers
  ];
  homepage = "http://dlaing.org/cofun";
  license = stdenv.lib.licenses.bsd3;
}
