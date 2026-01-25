{ mkDerivation, base, blaze-html, directory, lib, network-uri, sqlite-simple, unix }:
mkDerivation {
  pname = "qutebrowser-start-page";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base blaze-html directory network-uri sqlite-simple unix ];
  license = lib.licenses.mit;
  hydraPlatforms = lib.platforms.none;
}
