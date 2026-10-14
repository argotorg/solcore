// Allowances at an ERC-7201 namespaced slot. The raw write is module-private;
// approveAllowance and spendAllowance are the only exported mutators.
import * from std;
import {sload, sstore, log3, mstore} from std.opcodes;

export { allowanceOf, approveAllowance, spendAllowance };

function allowanceSlot(o : address, sp : address) returns (word) {
  return hash2(hash2(Typedef.rep(erc7201("mytoken.hooks.allowance")), Typedef.rep(o)), Typedef.rep(sp));
}

function allowanceOf(o : address, sp : address) returns (uint256) {
  return uint256(sload(allowanceSlot(o, sp)));
}

// module-private writer (not exported)
function writeAllowance(o : address, sp : address, v : uint256) returns (()) {
  sstore(allowanceSlot(o, sp), Typedef.rep(v));
}

function emitApproval(o : address, sp : address, value : uint256) returns (()) {
  let p : word = get_free_memory();
  mstore(p, Typedef.rep(value));
  log3(p, 32, keccakLit("Approval(address,address,uint256)"), Typedef.rep(o), Typedef.rep(sp));
}

function approveAllowance(o : address, sp : address, value : uint256) returns (()) {
  writeAllowance(o, sp, value);
  emitApproval(o, sp, value);
}

function spendAllowance(o : address, sp : address, value : uint256) returns (()) {
  let cur : uint256 = allowanceOf(o, sp);
  if (cur != Num.maxVal()) {
    require(cur >= value, Error(0xfb8f41b2));       // ERC20InsufficientAllowance
    writeAllowance(o, sp, Num.sub(cur, value));
  }
}
