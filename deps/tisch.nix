{ mkDerivation, aeson, base, bytestring, case-insensitive
, exceptions, fetchgit, lens, mtl, opaleye, postgresql-simple
, product-profunctors, profunctors, scientific, semigroups
, singletons, stdenv, tagged, text, time, transformers, uuid
}:
mkDerivation {
  pname = "tisch";
  version = "0.1";
  src = fetchgit {
    url = "https://github.com/k0001/tisch.git";
    sha256 = "15269q0p9gs73q6kwicalpdsqxkdj37xgpnb5rylbf02ah7qnp41";
    rev = "4d3fc55f0b64f50dc8a59b371e855a22d22f0a79";
  };
  libraryHaskellDepends = [
    aeson base bytestring case-insensitive exceptions lens mtl opaleye
    postgresql-simple product-profunctors profunctors scientific
    semigroups singletons tagged text time transformers uuid
  ];
  testHaskellDepends = [
    aeson base bytestring case-insensitive exceptions lens mtl opaleye
    postgresql-simple product-profunctors profunctors scientific
    semigroups singletons tagged text time transformers uuid
  ];
  homepage = "https://github.com/k0001/tisch";
  description = "Type-safe SQL interactions with PostgreSQL, based on Opaleye";
  license = stdenv.lib.licenses.asl20;
}
