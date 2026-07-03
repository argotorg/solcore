{
  description = "sol-core";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    foundry = {
      url = "github:shazow/foundry.nix/stable";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    goevmlab = {
      url = "github:holiman/goevmlab";
      flake = false;
    };
  };

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [ inputs.foundry.overlay ];
        };
        # One GHC version across native and the JS backend: 9.10. Keeping the
        # native compiler in step with the GHCJS cross-compiler avoids
        # base/version divergence (e.g. foldl' in Prelude) and lets precompiled
        # typecheck-cache dumps round-trip between the two.
        hspkgs = pkgs.haskell.packages.ghc910;

        gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
        sol-core = pkgs.haskell.lib.overrideCabal
          (hspkgs.callCabal2nix "sol-core" (gitignore ./.) { })
          (_: {
            # Keep package-level checks focused on unit tests.
            # Contract tests run in checks.contests where evmone/testrunner are provisioned.
            testTargets = [ "sol-core-tests" ];
          });
        sol-core-tests-no-warnings = pkgs.haskell.lib.overrideCabal sol-core
          (old: {
            buildTarget = "test:sol-core-tests";
            doHaddock = false;
            enableLibraryProfiling = false;
            checkPhase = ''
              runHook preCheck
              runHook postCheck
            '';
            configureFlags = (old.configureFlags or []) ++ [
              "--ghc-options=-Werror"
            ];
          });
        texlive = pkgs.texlive.combine { inherit (pkgs.texlive) scheme-small thmtools pdfsync lkproof cm-super; };
        evmone-lib = pkgs.callPackage ./nix/evmone.nix { };

        testrunner = pkgs.stdenv.mkDerivation {
          pname = "testrunner";
          version = "0.0";
          src = gitignore ./.;

          nativeBuildInputs = [ pkgs.cmake ];
          buildInputs = [ pkgs.boost pkgs.nlohmann_json ];

          cmakeFlags = [
            "-DIGNORE_VENDORED_DEPENDENCIES=ON"
          ];

          installPhase = ''
            mkdir -p $out/bin
            cp test/testrunner/testrunner $out/bin/
          '';
        };
      in
      rec {
        packages.sol-core = sol-core;
        packages.spec = pkgs.callPackage ./spec { solcoreTexlive = texlive; };
        packages.testrunner = testrunner;
        packages.evmone = evmone-lib;
        packages.tests-no-warnings = sol-core-tests-no-warnings;
        packages.default = packages.sol-core;

        apps.sol-core = inputs.flake-utils.lib.mkApp { drv = packages.sol-core; };
        apps.default = apps.sol-core;

        checks = {
          ormolu = pkgs.runCommand "ormolu-check" {
            buildInputs = [ hspkgs.ormolu ];
            src = gitignore ./.;
          } ''
            cd $src
            ormolu --mode check $(find app src yule test gen-std-cache -name '*.hs')
            touch $out
          '';

          contests = pkgs.stdenv.mkDerivation {
            pname = "solcore-contests";
            version = "0.0";
            src = gitignore ./.;

            nativeBuildInputs = [ pkgs.cmake ];
            buildInputs = [
              pkgs.boost
              pkgs.nlohmann_json
              sol-core
              pkgs.solc
              pkgs.jq
              pkgs.coreutils
              pkgs.bash
              evmone-lib
            ];

            cmakeFlags = [
              "-DIGNORE_VENDORED_DEPENDENCIES=ON"
            ];

            # Build testrunner
            buildPhase = ''
              cmake --build . --target testrunner
            '';

            checkPhase = ''
              cd ..
              export PATH=${sol-core}/bin:${pkgs.solc}/bin:${pkgs.jq}/bin:$PATH

              # Override commands and paths to use Nix-provided binaries
              export SOLCORE_CMD="sol-core"
              export YULE_CMD="yule"
              export testrunner_exe=build/test/testrunner/testrunner
              if [[ -f "${evmone-lib}/lib/libevmone.so" ]]; then
                export evmone=${evmone-lib}/lib/libevmone.so
              elif [[ -f "${evmone-lib}/lib/libevmone.dylib" ]]; then
                export evmone=${evmone-lib}/lib/libevmone.dylib
              else
                echo "libevmone shared library not found in ${evmone-lib}/lib" >&2
                exit 1
              fi

              # Run contest tests
              bash run_contests.sh
            '';

            installPhase = ''
              mkdir -p $out
              echo "Contests passed" > $out/result
            '';

            doCheck = true;
          };
        };

        devShells.default = hspkgs.shellFor {
          packages = _: [ sol-core ];
          buildInputs = [
            hspkgs.cabal-install
            hspkgs.haskell-language-server
            hspkgs.ormolu
            pkgs.boost
            pkgs.cmake
            pkgs.foundry-bin
            pkgs.go-ethereum
            pkgs.jq
            pkgs.nlohmann_json
            pkgs.solc
            evmone-lib
            (hspkgs.hevm.overrideAttrs (old: { patches = []; }))
            texlive
            (pkgs.callPackage ./nix/goevmlab.nix { src = inputs.goevmlab; })
            pkgs.mdbook
          ];
          evmone="${evmone-lib}/lib/${if pkgs.stdenv.isDarwin then "libevmone.dylib" else "libevmone.so"}";

          # Make sure the C++ testrunner is (re)built whenever its sources
          # change. CMake's incremental build is a no-op when nothing has
          # changed, so this is cheap on warm shells.
          #
          # CMakeCache.txt is deleted before each configure so that cmake
          # re-detects the compiler and make from the current nix store.
          # The cache becomes stale when the shell enters a different
          # derivation (different nix store hash for the same tool), causing
          # "no such file or directory" errors when the old path is gone.
          # Deleting the cache is safe: compiled object files are preserved
          # so the subsequent build is still incremental.
          shellHook = ''
            if [ -z "''${SOLCORE_SKIP_TESTRUNNER_BUILD:-}" ]; then
              testrunner_build_dir="''${PWD}/build"
              echo "[nix develop] Configuring testrunner build in $testrunner_build_dir"
              rm -f "$testrunner_build_dir/CMakeCache.txt"
              cmake -S "$PWD" -B "$testrunner_build_dir" \
                -DIGNORE_VENDORED_DEPENDENCIES=ON >/dev/null
              echo "[nix develop] Building testrunner (incremental)"
              cmake --build "$testrunner_build_dir" --target testrunner
            fi
          '';
        };
      }
    );
}
