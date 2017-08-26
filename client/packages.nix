{ mkDerivation, base, common, containers, lens, reflex, reflex-dom
, stdenv, text
}:
mkDerivation {
  pname = "client";
  version = "0.0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base common containers lens reflex reflex-dom text
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/uncannyworks/balanx#readme";
  description = "Balanx Client";
  license = stdenv.lib.licenses.asl20;
}
