{ lib, buildGoModule, src }:

buildGoModule {
  pname = "goevmlab";
  version = "0.0.1";

  inherit src;

  vendorHash = "sha256-TX+2Zl5I54si3Zp3Tdv9l79bKFgQ71gwukARYOZsg5Q=";

  subPackages = [
    "cmd/traceview"
    "cmd/tracediff"
  ];
}
