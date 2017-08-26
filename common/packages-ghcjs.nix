{ mkDerivation, aeson, base, bytestring, containers, data-default
, email-validate, exceptions, lens, mtl, reflex, reflex-dom, safe
, stdenv, string-conv, tagged, text, time, transformers
}:
mkDerivation {
  pname = "common";
  version = "0.0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers data-default email-validate
    exceptions lens mtl reflex reflex-dom safe string-conv tagged text
    time transformers
  ];
  homepage = "https://github.com/uncannyworks/balanx#readme";
  description = "Balanx Common";
  license = stdenv.lib.licenses.asl20;
}
