{ mkDerivation, adder-components, base, cofun-console
, cofun-coproduct, cofun-network, cofun-pairing, comonad, free, mtl
, network-simple, parsec, parsers, stdenv, transformers
}:
mkDerivation {
  pname = "adder-coproduct";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    adder-components base cofun-console cofun-coproduct cofun-network
    cofun-pairing comonad free mtl network-simple parsec parsers
    transformers
  ];
  homepage = "http://dlaing.org/cofun";
  license = stdenv.lib.licenses.bsd3;
}
