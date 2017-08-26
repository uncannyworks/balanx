{ reflex-platform ? import ../deps/reflex-platform {}
, ghcjs ? reflex-platform.ghcjs }:

let
  client = import ./. {
    inherit reflex-platform;
    inherit ghcjs;
  };
in
  client.env