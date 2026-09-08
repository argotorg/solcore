import reexport_module.pkg.api;

function main() returns (word) {
  return api.util.unwrap(api.util.Wrap.Mk(1));
}
