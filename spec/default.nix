{ stdenv, solcoreTexlive }:
stdenv.mkDerivation {
  src = ./.;
  pname = "sol-core-spec";
  version = "0.0.0";
  phases = ["unpackPhase" "buildPhase" "installPhase"];
  buildInputs = [ solcoreTexlive ];
  buildPhase = ''
    mkdir -p .cache/texmf-var
    # make some writable tempdirs
    export TEXMFHOME=.cache
    export TEXMFVAR=.cache/texmf-var
    make
  '';
  installPhase = ''
    mkdir -p $out
    cp new-solidity-spec.pdf $out/spec.pdf
  '';
}
