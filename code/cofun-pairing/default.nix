{ mkDerivation, base, comonad, free, stdenv, transformers }:
mkDerivation {
  pname = "cofun-pairing";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [ base comonad free transformers ];
  homepage = "https://dlaing.org/cofun";
  license = stdenv.lib.licenses.bsd3;
}
