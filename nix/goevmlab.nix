{ lib, buildGoModule, src }:

buildGoModule {
  pname = "goevmlab";
  version = "0.0.1";

  inherit src;

  vendorHash = "sha256-9+oisSe7AmGz+iwMQMzWSZFsAXbub11b1iVCqVnJX54=";

  subPackages = [
    "cmd/traceview"
    "cmd/tracediff"
  ];
}
