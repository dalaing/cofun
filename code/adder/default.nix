{ mkDerivation, base, cofun-pairing, comonad, free, mtl, stdenv
, transformers
}:
mkDerivation {
  pname = "adder";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [
    base cofun-pairing comonad free mtl transformers
  ];
  homepage = "http://dlaing.org/cofun";
  license = stdenv.lib.licenses.bsd3;
}
