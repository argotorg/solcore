{
  evmone = {
    owner = "ipsilon";
    repo = "evmone";
    rev = "6521d9d5012c936f0bf5f1a48668792caefe9b7a";
    hash = "sha256-yGspeBA4VhsOna+0VXEwShRNhi/apmrkw9Md8+P67DI=";
    fetchSubmodules = true;
  };

  intx = {
    owner = "chfast";
    repo = "intx";
    version = "0.14.0";
    rev = "v0.14.0";
    hash = "sha256-Comk1r5aLgvgFJofcHlENkOhvTYzMQhF5O6rbIwkGB0=";

    # Keep this aligned with evmone/cmake/Hunter/config.cmake.
    evmonePinPattern = "VERSION 0.14.0";
  };

  blst = {
    owner = "supranational";
    repo = "blst";
    version = "0.3.15";
    rev = "v0.3.15";
    hash = "sha256-Q9/zGN93TnJt2c8YvSaURstoxT02ts3nVkO5V08m4TI=";

    # Keep this aligned with evmone/cmake/blst.cmake.
    evmonePinPattern = "archive/refs/tags/v0.3.15.tar.gz";
  };
}
