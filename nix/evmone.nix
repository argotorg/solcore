{ lib, stdenv, cmake, fetchFromGitHub, intx, blst }:

let
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
    "-DEVMONE_TESTING=OFF"
    "-DBUILD_SHARED_LIBS=ON"
  ];

  # CMake handles installation automatically
}
