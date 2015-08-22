{ mkDerivation, base, binary, cofun-coproduct, cofun-pairing
, stdenv
}:
mkDerivation {
  pname = "cofun-network";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [ base binary cofun-coproduct cofun-pairing ];
  homepage = "http://dlaing.org/cofun";
  license = stdenv.lib.licenses.bsd3;
}
