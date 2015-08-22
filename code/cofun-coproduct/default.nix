{ mkDerivation, base, cofun-pairing, stdenv }:
mkDerivation {
  pname = "cofun-coproduct";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [ base cofun-pairing ];
  homepage = "https://dlaing.org/cofun";
  license = stdenv.lib.licenses.bsd3;
}
