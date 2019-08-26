{ mkDerivation, aeson, base, bytestring, containers, hpack
, postgresql-libpq, postgresql-simple, stdenv, text, time
, transformers, uuid
}:
mkDerivation {
  pname = "postgresql-dynamic";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers postgresql-libpq postgresql-simple
    text time transformers uuid
  ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  homepage = "https://github.com/bergey/postgresql-dynamic#readme";
  description = "Read rows from Postgres into 'Map Text Dynamic'";
  license = stdenv.lib.licenses.bsd3;
}
