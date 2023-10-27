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
    let makeHounds = system: { compiler ? "ghc928" }:
      let pkgs = nixpkgs.legacyPackages.${system};
          systemDepends = [ pkgs.lmdb ];
          call = compiler: pkgs.haskell.packages.${compiler}.callCabal2nixWithOptions;
          flags = "";
          src = builtins.path { path = ./.; name = "hounds-src"; };
          hounds_ = call compiler "hounds" src flags {};
      in
        pkgs.haskell.lib.overrideCabal hounds_ (oldAttrs: {
          executableSystemDepends = systemDepends;
          librarySystemDepends = systemDepends;
          testSystemDepends = systemDepends;
        });
    in
      flake-utils.lib.eachDefaultSystem (system:
        let hounds = makeHounds system;
        in {
          packages.hounds = hounds {};
          packages.default = self.packages.${system}.hounds;
        });
}
