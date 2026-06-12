Comptime for Core Solidity
===

# High-level overview

## Goals

## Use cases

### Storage offsets

Storage offsets are now computed dynamically, e.g:

```
      let offset : word = StorageSize.size(Proxy : Proxy(offsetType)) ;
      return storage(offset):storage(storageType);
```
where `StorageSize.size` adds sizes of all `offsetType` components.
Performing this at compilation time would give substantial gas savings.

```
forall a b. a:StorageSize, b:StorageSize => instance (a,b):StorageSize {
    function size(x:Proxy((a,b))) -> word {
        let a_sz:word = StorageSize.size(Proxy:Proxy(a));
        let b_sz:word = StorageSize.size(Proxy:Proxy(b));
        assembly {
            a_sz := add(a_sz, b_sz)
        }
        return a_sz;
    }
}
```


This example is interesting because:
- the function `size` is recursive, which shows comptime is more than simple inlining
- this function is overloaded and can be evaluated when specialised for concrete types, e.g.
    ```
    function StorageSize_size$word (x<Proxy(word)> : Proxy(word)) -> word {
      return 1;
   }

    function StorageSize_size$pairLa_bJ$word_pairLuint256_unitJ (x<Proxy((word, (uint256, ())))> : Proxy((word, (uint256, ())))) -> word {
      let a_sz<word> : word = StorageSize_size$word<Proxy(word) -> word>(Proxy<Proxy(word)>) ;
      let b_sz<word> : word = StorageSize_size$pairLa_bJ$uint256_unit<Proxy((uint256, ())) -> word>(Proxy<Proxy((uint256, ()))>) ;
      assembly {
         a_sz := add(a_sz, b_sz)
      }
      return a_sz<word>;
   }
    ```
- as defined, evaluating the function requires interpreting Yul assembly; an alternative might be using a builtin primitive for addition `primAddWord`.
  
### Method Dispatch (descriptors)

```
    function compute(prx : Proxy(Method(name,payability,args,rets,fn))) -> bytes4 {
        // setup memory pointers for the selector string
        let head = get_free_memory();
        let tail = Add.add(head, 32);

        // ensure the size is zero
        assembly { mstore(head, 0) }

        // write the selector string
        tail = ABIString.append(head, tail, Proxy : Proxy(name));
        tail = append_left_bracket(head, tail);
        tail = ABIString.append(head, tail, Proxy : Proxy(args));
        tail = append_right_bracket(head, tail);

        // hash it & get the first four bytes
        let res : word;
        assembly {
            let hash := keccak256(add(head, 32), mload(head))
            res := shr(224, hash)
        }
        return bytes4(res);
    }
```

**TODO:** efficient dispatch imlementation using comptime

### Other (TODO)
-  [EIP-712](https://eips.ethereum.org/EIPS/eip-712) `typeHash` see e.g. https://github.com/transmissions11/solmate/blob/main/src/tokens/ERC20.sol#L166 - limited use, debatable whether `name` and `chainid` are known at comptime

## Inspirations

Zig comptime: [guide](https://zig.guide/language-basics/comptime/), [reference](https://ziglang.org/documentation/0.14.0/#comptime)

# Design

## Target design

### Basics
`comptime` triggers evaluation, e.g.

```
let offset = comptime StorageSize.size(Proxy : Proxy(offsetType)) ;
```

#### Alternative syntaxes (TBD):

`let comptime offset = ...`

omitting let would probably lead to parser conflicts, but we could perhaps use const:
```
const offset = StorageSize.size(Proxy : Proxy(offsetType)) ;
```

### Comptime Evaluation

Evaluation fails when it encounters a dependency on a value not known at compilation time, or need to perform a runtime-only operation (e.g. accessing storage); *TBD: memory*

Expressions not marked as comptime still *MAY* be evaluated at compile time - this is an optimisation also known as constant folding. In our initial implementation this transformation includes also some function inlining and if/match unrolling.

### Function annotations

Let us start with the `fib` example from Zig:

```
function fib(comptime n : word) -> comptime word {
   if(n < 2) { return n; } else {return fib(n-1) + fib(n-2); }
}

contract Fib {
  function main() -> word {
    let res : comptime word = fib(10);
    return res;
  }
}
```
the argument annotation `comptime n` means that this argument *MUST* be known at comptime - otherwise a compile error occurs. Thus `fib` can be called only at comptime.

On the other hand, `comptime` annotation on function result promises that the function result will be known at comptime if all arguments are known at comptime:

```
function fib2(n : word) -> comptime word {
   if(n < 2) then n else (fib(n-1) + fib(n-2)) 
}
```
means `fib2` can be called both at comptime and runtime (note no `comptime` annotation on `n` this time). 

In many cases this can be inferred, but the annotation make the analysis easier, especially for overloaded functions:

```
/* Positive: comptime through an overloaded (type class) function.
   Scale.scale takes a comptime factor; if factor == 1 it returns x
   unchanged (conditional evaluated at comptime since factor is comptime).
   mulWord is builtinPure, so multiplication of comptime values is comptime.
*/
import std;

forall a. class a : Scale {
  function scale(comptime factor : word, comptime x : a) -> comptime a;
}

instance word : Scale {
  function scale(comptime factor : word, comptime x : word) -> comptime word {
    if (factor == 1) {
      return x;
    } else {
      return x * factor;
    }
  }
}

contract ComptimeOverloadedOk {
  function main() -> word {
    let a : comptime word = Scale.scale(1, 32);
    let b : comptime word = Scale.scale(3, 10);
    return a + b;
  }
}
```

on the other hand

```
instance word : Scale {
  function scale(comptime factor : word, comptime x : word) -> comptime word {
    let base : word;
    assembly {
      base := sload(0)
    }
    return base + x * factor;
  }
}
```
should be rejected - the result of `scale` depends on an `sload` and is not known at comptime.

### Comptime and runtime contexts and expressions

Even before evaluation, we may need to decide whether an expression is evaluated in a comptime context (CTC) or runtime context (RTC) and this may influence desugaring.




#### Contexts
- all public contract functions (in this example: `main`) are considered to be evaluated in RTC;
- parameters declared as comptime are evaluated in CTC;
- if a function is declared to have comptime result and is evaluated in CTC, its parameters are evaluated in CTC
- other function contexts are considered runtime
 
#### Expressions
- literals are considered comptime expressions (CTE, meaning their value is known at comptime)
- the RHS of `let comptime` are CTE
- values of comptime parameters and local variables are comptime expressions
- if a function is declared to have comptime result and all its parameters ere comptime
- other expressions are RTE

#### Desugaring

##### Target design: `integer` as the literal type (Zig-style)

The target design follows Zig's `comptime_int` model: **integer literals have
type `integer`**, a compile-time-only, unlimited-precision type (see
`doc/comptime-integer.md`).  A literal stays `integer` as long as it is in a
comptime context.  When it crosses into a runtime context, the desugaring pass
inserts a coercion at the **use site**:

| Runtime type expected | Coercion inserted |
|---|---|
| `word` | `wordFromInteger(n)` — builtin, no import needed |
| `uint256`, etc. | `fromInteger(n)` — dispatched via `Num` class (std) |

An unannotated `let x = 42` gives `x : integer`; the coercion is deferred to
wherever `x` is used in a runtime context.

This mirrors Zig precisely: `const x = 42` gives `x` type `comptime_int`;
`const y: u64 = x` inserts the coercion automatically.

##### "Pay only for what you use"

**Bare literals are coerced automatically; non-literal comptime expressions
require explicit conversion.**

```solidity
let a : uint256 = 1;                          // compiler inserts fromInteger(1)
let b : uint256 = fromInteger(fib(10));        // fib(10) : integer — user writes fromInteger
```

`fib(10)` is not a literal; the compiler cannot know at the desugaring pass
whether it will be comptime or not without full type information.  The user
making the boundary explicit is the "pay for what you use" cost of a
non-trivial comptime expression.

##### Example (target behaviour)

```solidity
function main() -> uint256 {
  let a : uint256 = 1;              // 1 : integer, coerced: fromInteger(1)
  let b : comptime integer = 2 + 2; // integer arithmetic, stays integer
  let c : comptime uint256 = fromInteger(b);   // explicit: integer -> uint256
  let d : uint256 = 3 + 3;         // 3 : integer; + on integer; coerced at let
  let e : comptime word = keccakLit("foo" + "bar");  // comptime string op
  return c * c - fromInteger(4) * a * d + e;
}
```

##### First implementation: literals remain `word`

In the first implementation `tcLit (IntLit _) = return word` is unchanged.
This avoids breaking any existing tests before the desugaring pass exists.
The coercions needed for the `integer` PoC are written explicitly by the user:

```solidity
let x : integer = wordToInteger(42);   // explicit lift
let y : word    = wordFromInteger(x);  // explicit lower
```

Once the desugaring pass is in place, `wordToInteger`/`wordFromInteger` at
literal sites disappear; the pass inserts `fromInteger` at runtime use sites
instead.

##### Design trade-offs

**Early (untyped) vs late (typed) desugaring.**  Running the pass on the
untyped AST uses only explicit annotations and collected signatures; it cannot
use full type inference.  It handles the common cases (annotated `let`,
`return`, calls to known functions with declared types) but may miss argument
positions whose types are determined purely by inference.  Late desugaring
(post-typecheck) has full type information but complicates the type checker.
The chosen approach is early desugaring for the common cases, with explicit
`fromInteger` as the user-written fallback for anything the pass cannot resolve.

**`fromWord` during the transition.**  While literals are still `word`-typed
(first implementation), the desugaring pass uses `Num.fromWord` for non-`word`
numeric annotations.  This is a temporary measure; `fromWord` remains useful
for runtime `word` values being coerced to richer types, but `fromInteger` is
the canonical path in the target design.

##### Desugaring pass structure (two-stage)

1. **Signature collection** — walk `CompUnit Name`, harvest declared types of
   all functions (parameter types, return type, comptime flags) including class
   methods.

2. **Contextual transform** — walk the AST maintaining:
   - CTC/RTC context (start RTC; enter CTC on `comptime` parameters and
     `let comptime` bindings)
   - Expected type at each expression position (from `let` annotation, function
     return type, or parameter type from the signature table)

   At each integer literal where the expected type is a runtime type, insert the
   appropriate coercion.  In CTC with expected type `integer`: no coercion.

### Memory
Memory needs careful treatment. For example, we want to fully evaluate the following at comptime:
``` solidity
function storeLoad(x : word) -> word {
  let r : word;
  assembly {
    mstore(0, x)
    r := mload(0)
  }
  return r;
}

contract ComptimeAsmMem {
  function main() -> word {
    let res : comptime word = storeLoad(42);
    return res;
  }
}
```

on the other hand we do not want to evaluate

``` solidity

function get_free_memory() -> word {
    let res : word;
    assembly { res := mload(0x40) }
    return res;
}

forall ty . ty:ABIAttribs, ty:ABIEncode => function abi_encode(val : ty) -> memory(bytes) {
    let free = get_free_memory();
    let tail = ABIEncode.encodeInto(val, free, 0, Add.add(free, ABIAttribs.headSize(Proxy : Proxy(ty))));
    set_free_memory(tail);
    return memory(free);
}

contract RuntimeAsmMem {
  function main() -> word {
      let ret : word = abi_encode(42);
      returndatacopy(ret, 32)
  }
}
```

The proposed solution is to only evaluate memory access when in "comptime mode" which is triggered by `let x : comptime`.

On the other hand, we may allow arithmetic in "folding mode":

``` solidity
function addWord(l: word, r: word) -> word {
  let rw : word;
  assembly {
      rw := add(l,r);
  }
  return rw;
}

function zero () { 0 }
function one() { addWord(1, zero()) }

contract OneOne {
    function main() -> word { addWord(one(), one()) }
}
```
so that we get `main` that just returns 2.

### comptime meory allocation - TBD

where should comptime memory addresses  come from? Should we have some comptime initialisation routine setting up the free memory pointer at 0x40 and use allocation functions from stdlib? Or some other mechanism?

### Preventing comptime-runtime pointer leaks (TBD)

One of the concerns about comptime memory use is the following scenario:
1. a memory pointer is allocated at comptime
2. the pointer is then converted to int, which is included in compiled code
3. at runtime the int is used as a memory address again which can lead to memory corruption

After some thought, this scenario does not seem as worrying as at the first glance. 
Most importantly, it is step 3 (converting arbitrary int to a pointer) that is dangerous and it has little to do with comptime - it is a prime way of shooting yourself in the foot in other languages as well (notably C).

If we do want to be extra careful, we may separate the integer and pointer types in assembly blocks at comptime and disallow conversions between (perhaps with the exception of literal 0).

Another (admittedly hacky) way is to allow at comptime to use only memory above $2^{64}$. In practice, runtime use of this memory would not be possilbe due to gas.

### comptime strings

TODO - perhaps best handled with builtin ops for concatenation and Keccak - this would remove much of the headaches with comptime memory.

Examples:

``` solidity
contract StringLitOps {
  function main() {
    // concat folds to a string literal, enabling revert("...")
    let s : comptime string = concatLit("ab", "cd");
    revert(s);
  }
}

contract StringLitLen {
  function main() -> word {
    // strlenLit folds to a word
    return strlenLit("hello");
  }
}

contract StringLitKeccak {
  function main() {
    // keccakLit folds to a 256-bit word (EVM/Yul semantics)
    return keccakLit("abc");
  }
}
```
 
 where `concatLit`, `strlenLit` and `keccakLit` are builtin, comptime-only functions.
 
## First implementation
 - literals remain `word`-typed; no desugaring pass yet
 - no comptime pointer protection
 - `integer` type introduced as infrastructure with explicit `wordToInteger`/`wordFromInteger`

The target desugaring design (Zig-style: `integer` literals, coercions at
runtime use sites) is described in the Desugaring section above.
The `integer` type PoC implementation plan is in `doc/comptime-integer.md`.
