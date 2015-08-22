{ mkDerivation, base, binary, cofun-console, cofun-coproduct
, cofun-network, cofun-pairing, comonad, free, parsec, parsers
, stdenv, transformers
}:
mkDerivation {
  pname = "adder-components";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [
    base binary cofun-console cofun-coproduct cofun-network
    cofun-pairing comonad free parsec parsers transformers
  ];
  homepage = "http://dlaing.org/cofun";
  license = stdenv.lib.licenses.bsd3;
}
