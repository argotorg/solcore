/*
  This is a library for avx style vectorized computation on evm words.
  We pack many smaller types into an evm word and run arithimetic / boolean
  operations in parallel on all components.

  For the first iteration overflow is considered undefined behaviour, and
  encountering it will produce nonsense results.

  - add / sub / mul work out of the box
  - and / or / xor / not work out of the box
  - div:
    - in the general case we have to iterate
    - there are optimised paths possible for constant divisors (especially powers of two)

  - iszero v
    - (v - 1) & ~v, mask high bits, shift right
    - PUSH16 0x1111
      PUSH16 0xB0C0
      SUB
      PUSH16 0xB0C0
      NOT
      AND
      PUSH16 0x8888
      AND
      PUSH1 0x3
      SHR

    - TODO: claude says it has a solution

  - eq
    - componentwise sub, followed by a componentwise iszero ==

  - not is hard

  Overflow handling:

    - add: need one overflow bit between each component
    - mul: need a whole lane to the right of the each component for overflow
    - sub: need twos complement (i.e. one extra sign bit to the left)

    with the above we can detect overflow for arithmetic ops.

  If we have the overflow checks, then we implement the comparisons as follows:

    - a < b: a - b, mask away every but everything but sign bit, shift right 1.
    - a > b: b - a, same as above ...

    - a `leq` b: a > b,  1 - result
    - a `geq` b: b > a,  1 - result

  TODO:
    - sext
    - sar
    - smod
    - sdiv

    ┬───────────────────┬──────────────────┬──────────────────┬────────────────────┬───────────────────┐
    │                   │                  │                  │                    │                   │
    │  c11              │   c12            │   c13            │    c14             │   c15             │
    ┼───────────────────┼──────────────────┼──────────────────┼────────────────────┴───────────────────┘


                                               OP

    ┬───────────────────┬──────────────────┬──────────────────┬────────────────────┬───────────────────┐
    │                   │                  │                  │                    │                   │
    │  c21              │   c22            │   c23            │    c24             │   c25             │
    ┼───────────────────┼──────────────────┼──────────────────┼────────────────────┴───────────────────┘


    produces:

    ┬───────────────────┬──────────────────┬──────────────────┬────────────────────┬───────────────────┐
    │                   │                  │                  │                    │                   │
    │  op(c11,c21)      │   op(c12,c22)    │   op(c13,c23)    │    op(c14,c24)     │   op(c15,c25)     │
    ┼───────────────────┼──────────────────┼──────────────────┼────────────────────┴───────────────────┘

    refs:
      - https://www.evm.codes/
      - https://graphics.stanford.edu/~seander/bithacks.html
  */

// --- type level numbers (binary rep) ---

/*
  note: this is kinda backwards

  0 (0)  -> zero
  1 (1)  -> one
  2 (10) -> bit0(one)
  3 (11) -> bit1(one)
*/

data zero;
data one;
data bit0(n);
data bit1(n);

class self:Number {}
class (self:Number) => self:NonZeroNumber {}

instance zero:Number {}
instance one:Number {}
instance one:NonZeroNumber {}
instance (n:NonZeroNumber) => bit0(n):Number {}
instance (n:NonZeroNumber) => bit0(n):NonZeroNumber {}
instance (n:NonZeroNumber) => bit1(n):Number {}
instance (n:NonZeroNumber) => bit1(n):NonZeroNumber {}

// --- conversion from type level to runtime words ---

// note: we
class self:FromNumber(result) {
    function get(_:Proxy(self)) -> result;
}

forall n:Number (n,t):FromNumber . function fromNum(x:n) -> t {
    return FromNumber.get(TYPE((n,t)));
}

data Proxy(t) = Proxy

data uint4 = uint4(word)

data ParVec(t, len) = ParVec(word)

class ty:BitWidth {
  function width(prx: Proxy t) -> word;
}

instance uint32:BitWidth {
  function width(_) {
    return 32;
  };
}

forall ty . BitWidth ty, len => class ParVec(ty, len):OnesVec(ty, len)  {
  function mask(prx: Proxy t) -> word
}

function iszero(v : ParVec(t,len)) -> ParVec(t,len) {
  let ones : ParVec := OnesVec.mask(prx : Proxy(ParVec(t,len)));
  assembly {

  }
}
