with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellPackages.override {
      overrides = self: super: {
        cofun-pairing = self.callPackage ../cofun-pairing {};
        adder = self.callPackage ./. {};
      };
    };
in modifiedHaskellPackages.adder.env

