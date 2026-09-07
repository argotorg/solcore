import {unwrap, Wrap} from reexport_items.pkg.api;

function main() returns (word) {
  return unwrap(Wrap.Mk(1));
}
