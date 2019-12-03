{ mkDerivation, aeson, base, binary, bytestring, concurrent-extra
, containers, directory, filepath, free, hpack, http-types, lens
, mtl, safe, scotty, serialport, stdenv, text, unix, vector, wai
, wai-app-static, wai-websockets, warp, websockets
}:
mkDerivation {
  pname = "puppet-master";
  version = "0.2.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base binary bytestring concurrent-extra containers directory
    filepath free http-types lens mtl safe scotty serialport text unix
    vector wai wai-app-static wai-websockets warp websockets
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base binary bytestring concurrent-extra containers directory
    filepath free http-types lens mtl safe scotty serialport text unix
    vector wai wai-app-static wai-websockets warp websockets
  ];
  preConfigure = "hpack";
  homepage = "https://github.com/eguahlak/puppetry#readme";
  description = "A puppetry webserver";
  license = stdenv.lib.licenses.mit;
}
