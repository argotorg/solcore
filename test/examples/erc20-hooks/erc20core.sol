// Core ledger (balances + total supply) at ERC-7201 namespaced slots.
import * from std;
import {sload, sstore, log3, mstore} from std.opcodes;
import * from option;
import * from hooks;

export { balance, supply, apply };

function supplySlot() returns (word) {
  return Typedef.rep(erc7201("mytoken.hooks.erc20.supply"));
}

function balanceSlot(who : address) returns (word) {
  return hash2(Typedef.rep(erc7201("mytoken.hooks.erc20.balances")), Typedef.rep(who));
}

function balance(who : address) returns (uint256) {
  return uint256(sload(balanceSlot(who)));
}

function supply() returns (uint256) {
  return uint256(sload(supplySlot()));
}

// --- module-private writers (not exported) ---
function move(from : address, to : address, amount : uint256) returns (()) {
  let bf : uint256 = balance(from);
  require(bf >= amount, Error(0xe450d38c));        // ERC20InsufficientBalance
  sstore(balanceSlot(from), Typedef.rep(Num.sub(bf, amount)));
  sstore(balanceSlot(to), Typedef.rep(Num.add(balance(to), amount)));
}

function mintTo(to : address, amount : uint256) returns (()) {
  let ts : uint256 = supply();
  let nts : uint256 = Num.add(ts, amount);
  require(nts >= ts, Error(0x4e487b71));           // overflow (Panic 0x11-like)
  sstore(supplySlot(), Typedef.rep(nts));
  sstore(balanceSlot(to), Typedef.rep(Num.add(balance(to), amount)));
}

function burnFrom(from : address, amount : uint256) returns (()) {
  let bf : uint256 = balance(from);
  require(bf >= amount, Error(0xe450d38c));        // ERC20InsufficientBalance
  sstore(balanceSlot(from), Typedef.rep(Num.sub(bf, amount)));
  sstore(supplySlot(), Typedef.rep(Num.sub(supply(), amount)));
}

function addrOf(o : Option<address>) returns (address) {
  return unwrapOr(o, address(0));
}

function emitTransfer(from : address, to : address, value : uint256) returns (()) {
  let p : word = get_free_memory();
  mstore(p, Typedef.rep(value));
  log3(p, 32, keccakLit("Transfer(address,address,uint256)"), Typedef.rep(from), Typedef.rep(to));
}

// The single sanctioned ledger mutator: hook chain first, then the effect.
// from = None mints, to = None burns.
function apply<h>(hooks : h, from : Option<address>, to : Option<address>, amount : uint256) returns (())
  where h: TransferHook {
  TransferHook.on(hooks, from, to, amount);
  match (from) {
    case Option.Some(src) {
      match (to) {
        case Option.Some(dst) { move(src, dst, amount); }
        default { burnFrom(src, amount); }
      }
    }
    default {
      match (to) {
        case Option.Some(dst) { mintTo(dst, amount); }
        default { require(false, Error(0xffffffff)); }   // empty update (unreachable)
      }
    }
  }
  emitTransfer(addrOf(from), addrOf(to), amount);
}
