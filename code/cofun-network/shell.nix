with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellPackages.override {
      overrides = self: super: {
        cofun-pairing = self.callPackage ../cofun-pairing {};
        cofun-coproduct = self.callPackage ../cofun-coproduct {};
        cofun-network = self.callPackage ./. {};
      };
    };
in modifiedHaskellPackages.cofun-network.env

