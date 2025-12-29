{ lib, stdenv, cmake, fetchFromGitHub }:

stdenv.mkDerivation {
  pname = "intx";
  version = "0.14.0";

  src = fetchFromGitHub {
    owner = "chfast";
    repo = "intx";
    rev = "v0.14.0";
    hash = "sha256-Comk1r5aLgvgFJofcHlENkOhvTYzMQhF5O6rbIwkGB0=";
  };

  nativeBuildInputs = [ cmake ];

  cmakeFlags = [
    "-DINTX_TESTING=OFF"
    "-DINTX_BENCHMARKING=OFF"
  ];
}
