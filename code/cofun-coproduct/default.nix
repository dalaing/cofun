{ mkDerivation, base, binary, cofun-pairing, stdenv }:
mkDerivation {
  pname = "cofun-coproduct";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base binary cofun-pairing ];
  homepage = "https://dlaing.org/cofun";
  license = stdenv.lib.licenses.bsd3;
}
