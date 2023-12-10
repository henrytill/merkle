{
  description = "It's in the tries...";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    let makeMerkle = system: { compiler ? "ghc948" }:
      let pkgs = nixpkgs.legacyPackages.${system};
          systemDepends = [ pkgs.lmdb ];
          call = compiler: pkgs.haskell.packages.${compiler}.callCabal2nixWithOptions;
          flags = "";
          src = builtins.path { path = ./.; name = "merkle-src"; };
          merkle_ = call compiler "merkle" src flags {};
      in
        pkgs.haskell.lib.overrideCabal merkle_ (oldAttrs: {
          executableSystemDepends = systemDepends;
          librarySystemDepends = systemDepends;
          testSystemDepends = systemDepends;
        });
    in
      flake-utils.lib.eachDefaultSystem (system:
        let merkle = makeMerkle system;
        in {
          packages.merkle = merkle {};
          packages.default = self.packages.${system}.merkle;
        });
}
