{ mkDerivation, base, binary, bytestring, cofun-coproduct
, cofun-pairing, comonad, exceptions, free, mmorph, mtl
, network-simple, stdenv
}:
mkDerivation {
  pname = "cofun-network";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base binary bytestring cofun-coproduct cofun-pairing comonad
    exceptions free mmorph mtl network-simple
  ];
  homepage = "http://dlaing.org/cofun";
  license = stdenv.lib.licenses.bsd3;
}
