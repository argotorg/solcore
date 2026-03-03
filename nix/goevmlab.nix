{ lib, buildGoModule, src }:

buildGoModule {
  pname = "goevmlab";
  version = "0.0.1";

  inherit src;

  vendorHash = "sha256-qSMcoQeDZNcxBKLkPbaGF69CtrJBAbm3VRHg7h23I5Y=";

  subPackages = [
    "cmd/traceview"
    "cmd/tracediff"
  ];
}
