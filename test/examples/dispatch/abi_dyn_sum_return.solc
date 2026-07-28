import std.{*};
import std.dispatch.{*};
import std.Generic.{*};
import std.ABIGeneric.{*};

// Return a *dynamic sum by value* from a dispatched function — the case the
// generic ABI encoder used to get wrong (it wrote only the top-level tag word,
// collapsing the whole value to a single 0x00…0 head). The companion
// `abi_dyn_sum.solc` deliberately avoids this by returning `memory(bytes)` /
// individual words; here we exercise the fixed `sum(f,g):ABIEncode` head-offset
// path head-on.
//
//   D2 : sum(uint256, bytes)                 -- dynamic (R carries bytes)
//   D3 : sum(uint256, sum(uint256, bytes))   -- dynamic, deeply right-nested
//   S2 : sum(uint256, uint256)               -- static  (control: inline, no offset)
//
// A dynamic sum is referenced by a 32-byte offset and laid out inline in the
// tail as [tag][branch]; each nested dynamic sum level emits its own offset
// word, so a deeply nested variant encodes as nested offsets, not flat tags.
// A static sum stays inline as [tag][branch] with no leading offset — its wire
// form is unchanged by the fix.
data D2 = L(uint256) | R(memory(bytes));
data D3 = X(uint256) | Y(uint256) | Z(memory(bytes));
data S2 = P(uint256) | Q(uint256);

contract DynSumRet {
  constructor() {}

  // ── shallow dynamic sum ────────────────────────────────────────────────
  // inl branch (static uint256 payload) of a dynamic sum: still takes the
  // dynamic encode path (offset word + inline [tag][value] in the tail).
  public function makeL(n : uint256) -> D2 {
    return D2.L(n);
  }

  // inr branch carrying a dynamic bytes payload: [off][1][off][len][data].
  public function makeR(b : memory(bytes)) -> D2 {
    return D2.R(b);
  }

  // ── deeply right-nested dynamic sum ────────────────────────────────────
  // outer inl: [off][0][value]
  public function makeX(n : uint256) -> D3 {
    return D3.X(n);
  }

  // inr(inl …): two dynamic-sum levels, so two nested offsets: [off][1][off][0][value]
  public function makeY(n : uint256) -> D3 {
    return D3.Y(n);
  }

  // inr(inr bytes): nested offsets down to the bytes leaf:
  // [off][1][off][1][off][len][data]
  public function makeZ(b : memory(bytes)) -> D3 {
    return D3.Z(b);
  }

  // ── static sum control ─────────────────────────────────────────────────
  // Byte-identical to the pre-fix output: inline [tag][value], no offset word.
  public function makeP(n : uint256) -> S2 {
    return S2.P(n);
  }

  public function makeQ(n : uint256) -> S2 {
    return S2.Q(n);
  }
}
