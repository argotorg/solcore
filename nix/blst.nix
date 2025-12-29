{ lib, stdenv, fetchFromGitHub }:

stdenv.mkDerivation {
  pname = "blst";
  version = "0.3.15";

  src = fetchFromGitHub {
    owner = "supranational";
    repo = "blst";
    rev = "v0.3.15";
    hash = "sha256-Q9/zGN93TnJt2c8YvSaURstoxT02ts3nVkO5V08m4TI=";
  };

  buildPhase = ''
    ./build.sh
  '';

  installPhase = ''
    mkdir -p $out/lib $out/include
    cp libblst.a $out/lib/
    cp bindings/blst.h bindings/blst_aux.h $out/include/
  '';
}
