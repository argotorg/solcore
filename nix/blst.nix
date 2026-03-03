{ lib, stdenv, fetchFromGitHub, pin ? (import ./evmone-pins.nix).blst }:

stdenv.mkDerivation {
  pname = "blst";
  version = pin.version;

  src = fetchFromGitHub {
    owner = pin.owner;
    repo = pin.repo;
    rev = pin.rev;
    hash = pin.hash;
  };

  buildPhase = ''
    ./build.sh
  '';

  installPhase = ''
    mkdir -p $out/lib $out/include
    cp libblst.a $out/lib/
    cp bindings/blst.h bindings/blst_aux.h $out/include/
  '';

  passthru = {
    inherit pin;
  };
}
