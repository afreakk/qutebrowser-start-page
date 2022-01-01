{ mkDerivation, base, blaze-html, lib, sqlite-simple, unix }:
mkDerivation {
  pname = "qutebrowser-start-page";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base blaze-html sqlite-simple unix ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
