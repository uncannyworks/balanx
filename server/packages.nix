{ mkDerivation, base, bytestring, cereal, common, containers
, cryptonite, exceptions, lens, logging-effect, mtl, opaleye
, postgresql-simple, product-profunctors, profunctors
, resource-pool, safe, scrypt, servant-auth-cookie, servant-server
, stdenv, text, time, tisch, transformers, uuid, wai, wai-cors
, warp, wl-pprint-text
}:
mkDerivation {
  pname = "server";
  version = "0.0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cereal common containers cryptonite exceptions lens
    logging-effect mtl opaleye postgresql-simple product-profunctors
    profunctors resource-pool safe scrypt servant-auth-cookie
    servant-server text time tisch transformers uuid wai wai-cors warp
    wl-pprint-text
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/uncannyworks/balanx#readme";
  description = "Balanx Server";
  license = stdenv.lib.licenses.asl20;
}
