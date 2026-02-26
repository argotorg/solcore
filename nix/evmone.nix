{ lib, stdenv, cmake, fetchFromGitHub, callPackage }:

let
  # Keep these in sync with evmone's own CMake pins:
  # - cmake/Hunter/config.cmake (intx)
  # - cmake/blst.cmake (blst)
  intx = callPackage ./intx.nix { };
  blst = callPackage ./blst.nix { };

  intxVersion = "0.14.0";
  blstVersion = "0.3.15";

  src = fetchFromGitHub {
    owner = "ipsilon";
    repo = "evmone";
    rev = "6521d9d5012c936f0bf5f1a48668792caefe9b7a";
    fetchSubmodules = true;
    hash = "sha256-yGspeBA4VhsOna+0VXEwShRNhi/apmrkw9Md8+P67DI=";
  };
in
stdenv.mkDerivation {
  pname = "evmone";
  version = "unstable";

  inherit src;

  nativeBuildInputs = [ cmake ];
  buildInputs = [ intx blst ];

  # Submodules are already fetched via fetchFromGitHub.fetchSubmodules
  # Create dummy .git directory to satisfy CMakeLists.txt check
  # Disable Hunter package manager and use Nix-provided dependencies
  preConfigure = ''
    if ! grep -q "VERSION ${intxVersion}" cmake/Hunter/config.cmake; then
      echo "evmone pinned intx version changed; update nix/intx.nix and nix/evmone.nix" >&2
      exit 1
    fi

    if ! grep -q "archive/refs/tags/v${blstVersion}.tar.gz" cmake/blst.cmake; then
      echo "evmone pinned blst version changed; update nix/blst.nix and nix/evmone.nix" >&2
      exit 1
    fi

    mkdir -p evmc/.git

    # Stub out Hunter to prevent network access
    cat > cmake/Hunter/init.cmake << 'EOF'
# Hunter disabled - using Nix dependencies
macro(hunter_add_package)
endmacro()
EOF

    # Stub out blst download to use Nix-provided blst
    cat > cmake/blst.cmake << EOF
# Using Nix-provided blst
add_library(blst::blst STATIC IMPORTED GLOBAL)
set_target_properties(
    blst::blst PROPERTIES
    INTERFACE_INCLUDE_DIRECTORIES "${blst}/include"
    IMPORTED_LOCATION "${blst}/lib/libblst.a"
)
EOF
  '';

  cmakeFlags = [
    # Keep tests off in this package; this avoids pulling Hunter-only test deps
    # (including nlohmann_json) into evmone runtime builds.
    "-DEVMONE_TESTING=OFF"
    "-DBUILD_SHARED_LIBS=ON"
  ];

  passthru = {
    inherit intx blst intxVersion blstVersion;
  };

  # CMake handles installation automatically
}
