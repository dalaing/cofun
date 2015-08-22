{ mkDerivation, base, cofun-coproduct, free, parsec, parsers
, stdenv, transformers
}:
mkDerivation {
  pname = "cofun-console";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [
    base cofun-coproduct free parsec parsers transformers
  ];
  homepage = "http://dlaing.org/cofun";
  license = stdenv.lib.licenses.bsd3;
}
