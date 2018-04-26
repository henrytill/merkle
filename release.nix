let

  pkgs = import <nixpkgs> {};

  systemDepends = [ pkgs.lmdb ];

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
