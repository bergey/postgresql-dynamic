{ mkDerivation, aeson, base, bytestring, containers
, postgresql-libpq, postgresql-simple, stdenv, text, time, uuid
}:
mkDerivation {
  pname = "postgresql-dynamic";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers postgresql-libpq postgresql-simple
    text time uuid
  ];
  homepage = "github.com/bergey/postgresql-dynamic";
  description = "Read rows from Postgres into 'Map Text Dynamic'";
  license = stdenv.lib.licenses.bsd3;
}
