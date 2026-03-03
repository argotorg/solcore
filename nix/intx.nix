{ lib, stdenv, cmake, fetchFromGitHub, pin ? (import ./evmone-pins.nix).intx }:

stdenv.mkDerivation {
  pname = "intx";
  version = pin.version;

  src = fetchFromGitHub {
    owner = pin.owner;
    repo = pin.repo;
    rev = pin.rev;
    hash = pin.hash;
  };

  nativeBuildInputs = [ cmake ];

  cmakeFlags = [
    "-DINTX_TESTING=OFF"
    "-DINTX_BENCHMARKING=OFF"
  ];

  passthru = {
    inherit pin;
  };
}
