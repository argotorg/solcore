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
        hspkgs = pkgs.haskell.packages.ghc98;

        gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
        sol-core = hspkgs.callCabal2nix "sol-core" (gitignore ./.) { };
        texlive = pkgs.texlive.combine { inherit (pkgs.texlive) scheme-small thmtools pdfsync lkproof cm-super; };
        intx = pkgs.callPackage ./nix/intx.nix { };
        blst = pkgs.callPackage ./nix/blst.nix { };
        evmone-lib = pkgs.callPackage ./nix/evmone.nix { inherit intx blst; };

        testrunner = pkgs.stdenv.mkDerivation {
          pname = "testrunner";
          version = "0.0";
          src = ./.;

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
        packages.intx = intx;
        packages.blst = blst;
        packages.evmone = evmone-lib;
        packages.default = packages.sol-core;

        apps.sol-core = inputs.flake-utils.lib.mkApp { drv = packages.sol-core; };
        apps.default = apps.sol-core;

        checks = {
          contests = pkgs.stdenv.mkDerivation {
            pname = "solcore-contests";
            version = "0.0";
            src = ./.;

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
              export evmone=${evmone-lib}/lib/libevmone${pkgs.stdenv.hostPlatform.extensions.sharedLibrary}

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
	     pkgs.boost
	     pkgs.cmake
             pkgs.foundry-bin
             pkgs.go-ethereum
             pkgs.jq
             pkgs.solc
             (hspkgs.hevm.overrideAttrs (old: { patches = []; }))
             texlive
             (pkgs.callPackage ./nix/goevmlab.nix { src = inputs.goevmlab; })
           ];
         };
      }
    );
}
