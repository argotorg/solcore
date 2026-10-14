// Transfer hooks: a hook runs before every balance change, and hooks compose
// as values (applied left to right). from = None is a mint, to = None a burn.
import * from std;
import * from option;

export { TransferHook, Stacked(*) };

trait TransferHook<h> {
  function on(hook : h, from : Option<address>, to : Option<address>, amount : uint256) returns (());
}

// A cons-cell of two hooks. The order is exactly how Stacked is nested at
// the use site.
enum Stacked<f, g> {
  Stacked(f, g)
}

impl<f, g> TransferHook<Stacked<f, g>> where f: TransferHook, g: TransferHook {
  function on(hook : Stacked<f, g>, from : Option<address>, to : Option<address>, amount : uint256) returns (()) {
    match (hook) {
      case Stacked(first, second) {
        TransferHook.on(first, from, to, amount);
        TransferHook.on(second, from, to, amount);
      }
    }
  }
}
