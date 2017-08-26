{ reflex-platform ? import ../deps/reflex-platform {}
, ghcjs ? reflex-platform.ghcjs }:

let
  common = ghcjs.callPackage ../common/packages-ghcjs.nix { };
  client = ghcjs.callPackage ./packages.nix {
    inherit common;
  };
in
  client