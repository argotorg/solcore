# Contracts

A contract is the unit of deployment. It groups a block of persistent _storage_
with the functions that operate on it, and it is the only construct that the
compiler turns into a callable on-chain entry point.

```solidity
contract Counter {
    count : uint256;

    constructor() {}

    function increment() public returns (uint256) {
        count = count + 1;
        return count;
    }

    function get() public returns (uint256) {
        return count;
    }
}
```

A contract body contains three kinds of member: _storage fields_, an optional
_constructor_, and _functions_. It may also declare contract-local data types and
type synonyms, which are visible only inside the contract.

## Storage fields

Each field declaration `name : T;` reserves persistent storage for a value of
type `T`. Fields are laid out in declaration order, and a field is referred to by
its bare name inside the contract's functions — there is no `this.` qualifier.
Reading and assigning a field desugar to `CanStore` operations on the field's
`storage<T>` reference (see [Data Locations](./data-locations.md)), so any type
with a `CanStore` instance can be a field, including mappings, arrays, and user
structs and enums.

```solidity
contract Ledger {
    owner    : address;
    balances : mapping(address => uint256);

    function balanceOf(a: address) public returns (uint256) {
        return balances[a];
    }
}
```

### Initialization

Storage starts out zero, so an unwritten field reads back as the all-zero slot:
`0` for a `uint256`, the zero address, an empty mapping. A constructor sets any
field that needs a non-zero initial value.

```solidity
contract Machine {
    ticks : uint256;   // starts at 0
    owner : address;   // starts at the zero address
    constructor(o: address) { owner = o; }
}
```

> **Note** For a word-backed type the zero slot is an unambiguous default. For a
> multi-constructor sum type it is less obviously meaningful: the zero slot
> decodes as the _first-declared_ constructor, a value that would change if the
> type's constructors were reordered. Initializing such a field explicitly in the
> constructor avoids relying on that.

## The constructor

The optional `constructor` runs once, at deployment, and is not part of the
contract's callable interface. It takes the deployment arguments, initializes
storage, and then disappears; its code is not retained in the deployed contract.

```solidity
contract Owned {
    owner : address;
    constructor(initialOwner: address) {
        owner = initialOwner;
    }
}
```

A contract with no constructor is deployed with an empty one. Unlike Classic
Solidity, there is no hierarchy of base constructors and no implicit ordering
between them: a contract initializes exactly the state it declares, in the order
its constructor writes it.

## Functions and visibility

A function marked `public` becomes part of the contract's external interface: it
is reachable through a call whose selector matches the function's signature. A
function without `public` is _internal_ — callable from other functions in the
contract but not exposed as an entry point. Functions may also be marked
`payable` to accept value; a non-`payable` public function rejects a call that
carries value.

Because a contract exposes behavior through its public functions rather than a
shared vtable, there is no `virtual`/`override`/`super`. Code is reused by
calling free functions and trait methods (see [Type Classes](../sail/typeclasses.md)),
and state is composed from modules and records rather than inherited.

## The generated entry point

The compiler synthesizes the contract's dispatcher automatically (it can be
disabled with `--no-gen-dispatch`). On each call the generated code reads the
incoming four-byte selector, compares it against each public method's
compile-time selector, decodes the remaining calldata into the method's
arguments, calls the method, and encodes the result as the return data. If no
selector matches, or the calldata is too short, control passes to the contract's
`fallback`. The full mechanism — selectors, argument decoding, and the
`Method`/`invokable` representation of a method — is described in
[ABI Encoding / Decoding](./abi-encoding-decoding.md#contract-dispatch).

The same signature information drives the Solidity-style ABI JSON the compiler
emits next to the bytecode, so existing tooling sees a familiar interface.

## Talking to other contracts

A contract calls another through a hand-built external call: compute the
selector, ABI-encode the arguments, and pass the payload to `raw_call`. There is
not yet a typed external-interface syntax (`IERC20(addr).transfer(…)`). The
[Cross-contract Calls](./cross-contract-calls.md) chapter works a complete
two-contract example through this pattern.
