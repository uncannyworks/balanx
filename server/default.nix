let
  pkgs = import <nixpkgs> { };
  tisch = pkgs.haskellPackages.callPackage ../deps/tisch.nix { };
  common = pkgs.haskellPackages.callPackage ../common/packages-ghc.nix { inherit tisch; };
  server = pkgs.haskellPackages.callPackage ./packages.nix {
    inherit common;
    inherit tisch;
  };
in
  server