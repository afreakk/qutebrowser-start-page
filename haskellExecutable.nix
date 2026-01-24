{ mkDerivation, base, blaze-html, lib, network-uri, sqlite-simple, unix }:
mkDerivation {
  pname = "qutebrowser-start-page";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base blaze-html network-uri sqlite-simple unix ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
