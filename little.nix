{ mkDerivation, attoparsec, base, bytestring, directory, lens, lib
, mtl, optparse-applicative, process, unordered-containers, xeno
}:
mkDerivation {
  pname = "little";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    attoparsec base bytestring directory lens mtl optparse-applicative
    process unordered-containers xeno
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
