let

  config = {
    packageOverrides = super: let self = super.pkgs; in {
      haskell =
        let
          lib = self.haskell.lib;
        in
          super.haskell // {
            packages = super.haskell.packages // {
              ghc822 = super.haskell.packages.ghc822.override {
                overrides = self: super: {
                  serialise = lib.overrideCabal super.serialise (oldAttrs: { doCheck = false; });
                };
              };
            };
          };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

  systemDepends = [ pkgs.lmdb ];

  # libraryPath = pkgs.stdenv.lib.makeLibraryPath systemDepends;

  jobs = rec {

    env = (hounds {}).env;

    hounds =
      { compiler ? "ghc822" }:
      let
        houndsRaw = pkgs.haskell.packages.${compiler}.callCabal2nix "hounds" ./. {};
      in
        pkgs.haskell.lib.overrideCabal houndsRaw (oldAttrs: {
          executableSystemDepends = systemDepends;
          librarySystemDepends    = systemDepends;
          testSystemDepends       = systemDepends;
        });
    };
in
  jobs
