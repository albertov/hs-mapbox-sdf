{ lib, mkDerivation, base, freetype, boost, inline-c, inline-c-cpp
, proto-lens, proto-lens-protoc, stdenv, template-haskell
, bytestring, hspec, hspec-expectations, lens, data-default
, protolude
}:
mkDerivation {
  pname = "hs-mapbox-sdf";
  version = "0.1.0.0";
  src = lib.cleanSource ./.;
  testHaskellDepends = [ hspec hspec-expectations ];
  libraryHaskellDepends = [
    base inline-c inline-c-cpp proto-lens proto-lens-protoc
    template-haskell bytestring lens data-default
    protolude
  ];
  configureFlags = [
    "--extra-lib-dirs=${freetype}/lib"
    "--extra-include-dirs=${freetype}/include"
    "--extra-include-dirs=${boost.dev}/include"
    ];
  shellHook="export BOOST_INC=${boost.dev}/include";
  librarySystemDepends = [ freetype ];
  homepage = "http://github.com/albertov/hs-mapbox-sdf";
  license = stdenv.lib.licenses.bsd3;
}
