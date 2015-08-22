with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellPackages.override {
      overrides = self: super: {
        cofun-pairing = self.callPackage ../cofun-pairing {};
        cofun-coproduct = self.callPackage ../cofun-coproduct {};
        cofun-console = self.callPackage ../cofun-console {};
        adder-components = self.callPackage ../adder-components {};
        adder-coproduct = self.callPackage ./. {};
      };
    };
in modifiedHaskellPackages.adder-coproduct.env

