{
  description = "sol-core";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    foundry = {
      url = "github:shazow/foundry.nix/stable";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hevm = {
      url = "github:ethereum/hevm";
      inputs.nixpkgs.follows = "nixpkgs";
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
        hspkgs = pkgs.haskell.packages.ghc982;

        gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
        sol-core = hspkgs.callCabal2nix "sol-core" (gitignore ./.) { };
        texlive = pkgs.texlive.combine { inherit (pkgs.texlive) scheme-small thmtools pdfsync lkproof cm-super; };
      in
      rec {
        packages.sol-core = sol-core;
        packages.spec = pkgs.callPackage ./spec { solcoreTexlive = texlive; };
        packages.default = packages.sol-core;

        apps.sol-core = inputs.flake-utils.lib.mkApp { drv = packages.sol-core; };
        apps.default = apps.sol-core;

        devShells.default = hspkgs.shellFor {
          packages = _: [ sol-core ];
          buildInputs = [
            inputs.hevm.packages.${system}.default
            hspkgs.cabal-install
            hspkgs.haskell-language-server
            pkgs.foundry-bin
            pkgs.solc
            texlive
          ];
        };
      }
    );
}
