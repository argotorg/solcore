{
  description = "sol-core";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    foundry.url = "github:shazow/foundry.nix/stable";
  };

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [ inputs.foundry.overlay ];
        };
        hspkgs = pkgs.haskell.packages.ghc982;

        gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
        sol-core = (hspkgs.callCabal2nix "sol-core" (gitignore ./.) {})
          .overrideAttrs (attrs: {
            buildInputs = attrs.buildInputs ++ [ hspkgs.ghc hspkgs.cabal-install ];
            preCheck = (attrs.preCheck or "") + ''
              export SOL_CORE_EXE=$(realpath $(find . -name "sol-core" -type f -executable | head -1))
              export SOLVER_EXE=$(realpath $(find . -name "solver" -type f -executable | head -1))
            '';
          });
      in rec {
        packages.sol-core = sol-core;
        packages.default = packages.sol-core;

        apps.act = inputs.flake-utils.lib.mkApp { drv = packages.sol-core; };
        apps.default = apps.act;

        devShells.default = hspkgs.shellFor {
          packages = _: [ sol-core ];
          buildInputs = [
            hspkgs.haskell-language-server
            hspkgs.cabal-install
            pkgs.foundry-bin
            pkgs.solc
          ];
        };
      });
}
