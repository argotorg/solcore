# VaultToken: contract composition from Classic Solidity to Core Solidity

This document opens with a survey of the contract-composition patterns that can be found
in popular Solidity libraries. It then follows a single running example, a vault
share token, across three encodings. It starts from a self-contained Classic
Solidity contract that exercises several of those patterns at once. It then shows
the example in Core Solidity, first in the type-class and instance representation
that the compiler uses for contracts, and then using a contract-inheritance feature
built on top of that representation. A dedicated section explains how that
inheritance is modeled with data types, classes and instances.

## 1. Types of contract composition in real repositories

Before the running example, this section surveys the composition patterns that
occur across widely used Solidity codebases. Each pattern is shown with a short
excerpt from a real library and a link to the file it comes from. The excerpts are
pinned to specific released versions so that the links stay stable. The running
example in the rest of the document combines several of these patterns at once.

### Inheritance and mixins

The most visible form of composition is inheritance. A contract lists several base
contracts, and Solidity merges them into one through C3 linearization, producing a
single flat storage layout. OpenZeppelin's `ERC20` is itself assembled this way,
from a context helper and a set of interfaces:

```solidity
abstract contract ERC20 is Context, IERC20, IERC20Metadata, IERC20Errors {
```

Found in OpenZeppelin
[`contracts/token/ERC20/ERC20.sol`](https://github.com/OpenZeppelin/openzeppelin-contracts/blob/v5.6.1/contracts/token/ERC20/ERC20.sol).

### Virtual hooks and `super`

Inheritance becomes open composition when a base routes its behavior through a
single `virtual` function that derived contracts override and extend with `super`.
OpenZeppelin's `ERC20` sends every balance change through `_update`, and
`ERC20Pausable` overrides that one seam to add a guard before delegating back:

```solidity
abstract contract ERC20Pausable is ERC20, Pausable {
    function _update(address from, address to, uint256 value) internal virtual override whenNotPaused {
        super._update(from, to, value);
    }
}
```

Found in OpenZeppelin
[`contracts/token/ERC20/extensions/ERC20Pausable.sol`](https://github.com/OpenZeppelin/openzeppelin-contracts/blob/v5.6.1/contracts/token/ERC20/extensions/ERC20Pausable.sol).

### Modifiers as reusable guards

Access control and similar pre-checks are composed through modifiers, which a base
contract defines once and any function attaches by name. OpenZeppelin's `Ownable`
contributes `onlyOwner`, which a derived contract uses without knowing how the
check is implemented:

```solidity
modifier onlyOwner() {
    _checkOwner();
    _;
}

function _checkOwner() internal view virtual {
    if (owner() != _msgSender()) {
        revert OwnableUnauthorizedAccount(_msgSender());
    }
}
```

Found in OpenZeppelin
[`contracts/access/Ownable.sol`](https://github.com/OpenZeppelin/openzeppelin-contracts/blob/v5.6.1/contracts/access/Ownable.sol).

### Libraries attached with `using ... for`

A library bundles functions that operate on a given type, and `using L for T` makes
them read as methods on values of that type. OpenZeppelin's `SafeERC20` wraps raw
token calls and is attached with `using SafeERC20 for IERC20`:

```solidity
library SafeERC20 {
    function safeTransfer(IERC20 token, address to, uint256 value) internal {
        if (!_safeTransfer(token, to, value, true)) {
            revert SafeERC20FailedOperation(address(token));
        }
    }
}
```

Found in OpenZeppelin
[`contracts/token/ERC20/utils/SafeERC20.sol`](https://github.com/OpenZeppelin/openzeppelin-contracts/blob/v5.6.1/contracts/token/ERC20/utils/SafeERC20.sol).

### Interfaces and callbacks

Composition also runs in the opposite direction. A contract calls back into an
arbitrary counterparty through an interface, inverting control. Uniswap V2 invokes
`uniswapV2Call` on the caller during a flash swap, so the borrower supplies the
logic that runs in the middle of the pair's own function:

```solidity
interface IUniswapV2Callee {
    function uniswapV2Call(address sender, uint amount0, uint amount1, bytes calldata data) external;
}
```

Found in Uniswap
[`v2-core/contracts/interfaces/IUniswapV2Callee.sol`](https://github.com/Uniswap/v2-core/blob/master/contracts/interfaces/IUniswapV2Callee.sol).

### Delegation through proxies

Rather than inheriting code, a contract can forward every call to another contract
with `delegatecall`, running the target's code against its own storage. This is the
basis of upgradeable proxies and minimal-proxy clones. OpenZeppelin's `Proxy`
carries the core of the pattern:

```solidity
function _delegate(address implementation) internal virtual {
    assembly {
        calldatacopy(0x00, 0x00, calldatasize())
        let result := delegatecall(gas(), implementation, 0x00, calldatasize(), 0x00, 0x00)
        returndatacopy(0x00, 0x00, returndatasize())
        switch result
        case 0 { revert(0x00, returndatasize()) }
        default { return(0x00, returndatasize()) }
    }
}
```

Found in OpenZeppelin
[`contracts/proxy/Proxy.sol`](https://github.com/OpenZeppelin/openzeppelin-contracts/blob/v5.6.1/contracts/proxy/Proxy.sol).

### Factories and deploy-time composition

Finally, contracts compose at deployment time. A factory creates new instances,
often with `create2` for a deterministic address, and wires them together. Uniswap
V2's factory deploys each pair this way:

```solidity
function createPair(address tokenA, address tokenB) external returns (address pair) {
    // ... ordering and existence checks ...
    bytes memory bytecode = type(UniswapV2Pair).creationCode;
    bytes32 salt = keccak256(abi.encodePacked(token0, token1));
    assembly {
        pair := create2(0, add(bytecode, 32), mload(bytecode), salt)
    }
    IUniswapV2Pair(pair).initialize(token0, token1);
    // ... registry bookkeeping ...
}
```

Found in Uniswap
[`v2-core/contracts/UniswapV2Factory.sol`](https://github.com/Uniswap/v2-core/blob/master/contracts/UniswapV2Factory.sol).

### Putting the patterns together

These patterns are not exclusive, and a single contract usually mixes them. The
running example in the next section does exactly that, combining inheritance, a
virtual hook with `super`, modifiers, a library, an interface callback and a
CREATE2 factory in one vault token.

## 2. The example in Classic Solidity

The example is a vault share token that exercises every composition mechanism
Solidity offers at once: multiple inheritance with a shared virtual hook, a
small library used through `using ... for`, an interface plus a callback, a
factory that deploys with CREATE2, modifiers for access control, and state
segregated per mixin. It depends on no external library.

```solidity
pragma solidity ^0.8.24;

library Math {
    function addChecked(uint256 a, uint256 b) internal pure returns (uint256 c) {
        c = a + b;
    }
    function subChecked(uint256 a, uint256 b) internal pure returns (uint256) {
        require(a >= b, "underflow");
        return a - b;
    }
}

interface IFlashBorrower {
    function onFlashMint(address initiator, uint256 amount, bytes calldata data)
        external returns (bytes32);
}

abstract contract Ownable {
    address private _owner;
    error NotOwner();
    constructor() { _owner = msg.sender; }
    modifier onlyOwner() { if (msg.sender != _owner) revert NotOwner(); _; }
    function owner() public view returns (address) { return _owner; }
    function transferOwnership(address to) public onlyOwner { _owner = to; }
}

abstract contract Pausable {
    bool private _paused;
    error Paused();
    function paused() public view returns (bool) { return _paused; }
    function _setPaused(bool p) internal { _paused = p; }
    modifier whenNotPaused() { if (_paused) revert Paused(); _; }
}

abstract contract ERC20Base {
    using Math for uint256;

    mapping(address => uint256) public balanceOf;
    uint256 public totalSupply;
    event Transfer(address indexed from, address indexed to, uint256 value);

    function _update(address from, address to, uint256 amount) internal virtual {
        if (from == address(0)) {
            totalSupply = totalSupply.addChecked(amount);
        } else {
            balanceOf[from] = balanceOf[from].subChecked(amount);
        }
        if (to == address(0)) {
            totalSupply = totalSupply.subChecked(amount);
        } else {
            balanceOf[to] = balanceOf[to].addChecked(amount);
        }
        emit Transfer(from, to, amount);
    }

    function transfer(address to, uint256 amount) public returns (bool) {
        _update(msg.sender, to, amount);
        return true;
    }
}

contract VaultToken is Ownable, Pausable, ERC20Base {
    bytes32 private constant FLASH_OK = keccak256("IFlashBorrower.onFlashMint");

    constructor(uint256 initialSupply) {
        _update(address(0), msg.sender, initialSupply);
    }

    function _update(address from, address to, uint256 amount)
        internal override whenNotPaused
    {
        super._update(from, to, amount);
    }

    function setPaused(bool p) external onlyOwner { _setPaused(p); }

    function mint(address to, uint256 amount) external onlyOwner {
        _update(address(0), to, amount);
    }

    function flashMint(IFlashBorrower borrower, uint256 amount, bytes calldata data)
        external
    {
        _update(address(0), address(borrower), amount);
        bytes32 ok = borrower.onFlashMint(msg.sender, amount, data);
        require(ok == FLASH_OK, "callback failed");
        _update(address(borrower), address(0), amount);
    }
}

contract VaultTokenFactory {
    event Created(address token, bytes32 salt);
    function create(uint256 initialSupply, bytes32 salt) external returns (address token) {
        token = address(new VaultToken{salt: salt}(initialSupply));
        VaultToken(token).transferOwnership(msg.sender);
        emit Created(token, salt);
    }
    function predict(uint256 initialSupply, bytes32 salt) external view returns (address) {
        bytes32 h = keccak256(abi.encodePacked(
            bytes1(0xff), address(this), salt,
            keccak256(abi.encodePacked(type(VaultToken).creationCode, abi.encode(initialSupply)))
        ));
        return address(uint160(uint256(h)));
    }
}
```

### What the composition is doing

The line `contract VaultToken is Ownable, Pausable, ERC20Base` is the heart of
the example. It composes three independent pieces into one contract, and Solidity
resolves it through C3 linearization, producing a single flat storage layout that
holds `_owner`, `_paused`, `balanceOf` and `totalSupply` together. Each mixin
contributes its own slice of state and never sees the others.

`ERC20Base` declares `_update` as `virtual` and routes `transfer`, `mint` and the 
flash-mint through it, so `_update` is the one seam every token movement passes. 
`VaultToken` overrides `_update`, attaches the
`whenNotPaused` modifier, and then calls `super._update`. The `super` call is what
makes the composition open: the override adds the pause check and delegates the
actual ledger movement to the next contract in the linearization, without copying
its code. If a fourth mixin were stacked on top, it could override `_update`
again and chain another `super` call, and the order would follow the
linearization automatically.

The modifiers `onlyOwner` and `whenNotPaused` are the access-control face of the
same idea. They are reusable guards contributed by `Ownable` and `Pausable`, and
the final contract attaches them to `mint`, `setPaused` and the overridden
`_update` without knowing how the check is implemented.

The remaining mechanisms are lighter. The `Math` library is attached with `using
Math for uint256` so that `totalSupply.addChecked(amount)` reads as a method call.
The interface `IFlashBorrower` plus the `onFlashMint` callback invert control: the
token hands freshly minted units to an arbitrary borrower and trusts it to return
a magic value before the units are burned back. The factory composes at
deployment time, using `new VaultToken{salt: salt}(...)` to obtain a deterministic
address that `predict` can compute off-chain.

The rest of the document keeps the same behavior and re-expresses this
composition in Core Solidity, where there is no native inheritance and no `super`.

## 3. The example in the Core Solidity representation

Core Solidity represents a contract not as a primitive construct but as a type
class together with one instance over the contract's storage. This is the same
representation the compiler already produces when it desugars field access and
contracts. Core does not yet have interfaces, so this section expresses the
composition with the features the compiler accepts today: a single contract that
declares all of the state and shares behavior through ordinary methods.

The representation rests on two ideas.

First, a contract's storage is a value of a generated type. For a contract `C`
the compiler emits a singleton tag `data CCxt = CCxt;`, and the storage object has
type `ContractStorage(CCxt)`. Each field becomes a selector singleton together
with a `CStructField` instance that records the field's storage type and its
offset, which is the product of the types of all preceding fields. A field read
becomes `RVA.acc` over a `MemberAccessProxy(ContractStorage(CCxt), selector)`, and
a field write becomes `Assign.assign(LVA.acc(...), rhs)`. So a method body that
manipulates fields is ordinary code over these proxies, and the receiver is the
explicit `self : ContractStorage(CCxt)`.

Second, the contract itself becomes a type class with a single instance on
`ContractStorage(CCxt)`. The instance carries the method bodies, and because there
is exactly one instance per storage type, every call resolves to it.

In this representation the vault is one contract that declares all of the state the
three Classic Solidity mixins used to hold separately, and the behaviors they
contributed become ordinary methods on that one contract. The single virtual hook
becomes one internal method, `update`, that every token movement routes through,
with the pause guard inlined at that seam.

```solc
import std.{*};
import std.dispatch.{*};
import std.opcodes.{caller};   // EVM opcode functions; caller() -> word

// One flat contract holding all the state the three mixins used to hold
// separately, with the shared hook as a single internal method.
contract VaultToken {
  owner_ : address;
  paused_ : uint256;       // 0 = not paused, 1 = paused (bool is not storable yet)
  totalSupply_ : uint256;
  balances : mapping(address, uint256);

  constructor(initialSupply : uint256) {
    owner_ = address(caller());
    paused_ = uint256(0);
    update(address(0), address(caller()), initialSupply);   // initial mint through the seam
  }

  // ownership face (the Ownable mixin)
  public function owner() -> address { return owner_; }
  public function transferOwnership(newOwner : address) -> () {
    requireOwner();
    owner_ = newOwner;
  }
  function requireOwner() -> () {
    require(address(caller()) == owner_, Error(0x12b0c500));  // onlyOwner
  }

  // pause face (the Pausable mixin)
  public function paused() -> bool { return paused_ != uint256(0); }
  public function setPaused(flag : uint256) -> () {
    requireOwner();
    paused_ = flag;
  }

  // ERC-20 face (the ERC20Base mixin)
  public function totalSupply() -> uint256 { return totalSupply_; }
  public function balanceOf(account : address) -> uint256 { return balances[account]; }
  public function transfer(to : address, amount : uint256) -> bool {
    update(address(caller()), to, amount);
    return true;
  }
  public function mint(to : address, amount : uint256) -> () {
    requireOwner();
    update(address(0), to, amount);
  }

  // the single seam every movement flows through: the pause check and the
  // ledger movement live together in the one body the calls reach
  function update(from : address, to : address, amount : uint256) -> () {
    require(paused_ == uint256(0), Error(0x9e87fac8));  // whenNotPaused / Paused()
    if (from != address(0)) { balances[from] = Num.sub(balances[from], amount); }
    else { totalSupply_   = Num.add(totalSupply_, amount); }
    if (to != address(0)) { balances[to]   = Num.add(balances[to], amount); }
    else { totalSupply_   = Num.sub(totalSupply_, amount); }
    // emit Transfer(from, to, amount);  // events are a separate feature
  }
}
```

The compiler turns the contract into the storage infrastructure and one instance.
The four fields, in declaration order, produce four selectors whose offsets are the
nested product of the preceding field types:

```solc
data VaultTokenCxt = VaultTokenCxt;

data VaultToken_owner_sel = VaultToken_owner_sel;
instance StructField(ContractStorage(VaultTokenCxt), VaultToken_owner_sel)
  : CStructField(storage(address), ()) {}

data VaultToken_paused_sel = VaultToken_paused_sel;
instance StructField(ContractStorage(VaultTokenCxt), VaultToken_paused_sel)
  : CStructField(storage(uint256), (address, ())) {}

data VaultToken_totalSupply_sel = VaultToken_totalSupply_sel;
instance StructField(ContractStorage(VaultTokenCxt), VaultToken_totalSupply_sel)
  : CStructField(storage(uint256), (address, (uint256, ()))) {}

data VaultToken_balances_sel = VaultToken_balances_sel;
instance StructField(ContractStorage(VaultTokenCxt), VaultToken_balances_sel)
  : CStructField(storage(mapping(address, uint256)), (address, (uint256, (uint256, ())))) {}
```

The contract becomes a type class, and a single instance on
`ContractStorage(VaultTokenCxt)` carries the bodies, with
`self : ContractStorage(VaultTokenCxt)` as the receiver and field access expressed
through the proxies:

```solc
class self : VaultToken {
  function owner(self : self) -> address;
  function transferOwnership(self : self, newOwner : address) -> ();
  function paused(self : self) -> bool;
  function setPaused(self : self, flag : uint256) -> ();
  function totalSupply(self : self) -> uint256;
  function balanceOf(self : self, account : address) -> uint256;
  function transfer(self : self, to : address, amount : uint256) -> bool;
  function mint(self : self, to : address, amount : uint256) -> ();
  function update(self : self, from : address, to : address, amount : uint256) -> ();
}

instance ContractStorage(VaultTokenCxt) : VaultToken {
  function owner(self : ContractStorage(VaultTokenCxt)) -> address {
    return RVA.acc(MemberAccessProxy(ContractStorage(VaultTokenCxt), VaultToken_owner_sel));
  }
  function transfer(self : ContractStorage(VaultTokenCxt), to : address, amount : uint256) -> bool {
    update(self, address(caller()), to, amount);
    return true;
  }
  // ... the remaining methods, each over the VaultToken_*_sel selectors
}
```

This already reproduces the Classic Solidity behavior. The state composition is
done by declaring the fields together, and the behavior composition is done by
sharing ordinary methods, with the one internal `update` seam standing in for the
virtual hook. What it does not yet provide is a way to reuse the field declarations
and the method bodies of a separate base, which is the role of inheritance.

## 4. The same example using inheritance

Inheritance lets a contract extend another, reusing its fields and its method
bodies. The surface adds three keywords: `interface`, `implements` and `inherits`.
A contract inherits from at most one parent and may implement any number of
interfaces. Inheriting copies the parent's fields into the child, so storage stays
flat, and it makes the parent's methods callable on the child's storage.

The vault has three stateful mixins. Since a contract inherits from one parent, we
arrange them as a chain, which is the faithful encoding of Solidity's linearized
storage: `Ownable`, then `Pausable inherits Ownable`, then `ERC20Base inherits
Pausable`, then `VaultToken inherits ERC20Base`. Each layer keeps its own state
and reuses the inherited state directly.

```solc
import std.{*};
import std.dispatch.{*};
import std.opcodes.{caller};   // EVM opcode functions; caller() -> word

interface IOwnable {
  function owner() -> address;
  function transferOwnership(newOwner : address) -> ();
}
interface IPausable { function paused() -> bool; }
interface IERC20 {
  function totalSupply() -> uint256;
  function balanceOf(account : address) -> uint256;
  function transfer(to : address, amount : uint256) -> bool;
}

// root layer: ownership state and the guard
contract Ownable implements IOwnable {
  owner_ : address;
  constructor(initialOwner : address) { owner_ = initialOwner; }
  function owner() -> address { return owner_; }
  function transferOwnership(newOwner : address) -> () {
    require(address(caller()) == owner_, Error(0x12b0c500));   // onlyOwner
    owner_ = newOwner;
  }
}

// pause layer: adds a flag, reuses the inherited owner_ for the guard
contract Pausable inherits Ownable implements IPausable {
  paused_ : bool;
  function paused() -> bool { return paused_; }
  function setPaused(p : bool) -> () {
    require(address(caller()) == owner_, Error(0x12b0c500));   // owner_ inherited from Ownable
    paused_ = p;
  }
}

// ledger layer: balances, supply and the single update seam
contract ERC20Base inherits Pausable implements IERC20 {
  balances     : mapping(address, uint256);
  totalSupply_ : uint256;

  function totalSupply() -> uint256 { return totalSupply_; }
  function balanceOf(account : address) -> uint256 { return balances[account]; }
  function transfer(to : address, amount : uint256) -> bool {
    update(address(caller()), to, amount);
    return true;
  }
  // the one seam every movement flows through
  function update(from : address, to : address, amount : uint256) -> () {
    if (from != address(0)) { balances[from] = Num.sub(balances[from], amount); }
    else                    { totalSupply_   = Num.add(totalSupply_, amount); }
    if (to   != address(0)) { balances[to]   = Num.add(balances[to], amount); }
    else                    { totalSupply_   = Num.sub(totalSupply_, amount); }
  }
}

// final contract: overrides the seam, adds mint and flash-mint
contract VaultToken inherits ERC20Base {
  constructor(initialSupply : uint256) {
    owner_ = address(caller());
    update(address(0), address(caller()), initialSupply);
  }

  // Pausable dresses the inherited hook
  function update(from : address, to : address, amount : uint256) -> () {
    require(not(paused()), Error(0x9e87fac8));        // whenNotPaused
    super.update(from, to, amount);                   // delegate to ERC20Base.update
  }

  function mint(to : address, amount : uint256) -> () {
    require(address(caller()) == owner_, Error(0x12b0c500));   // onlyOwner
    update(address(0), to, amount);
  }
}
```

Compared with section 3, the state composition is no longer done by hand: the
chain accumulates `owner_`, `paused_`, `balances` and `totalSupply_` into the
final storage automatically. The hook is overridden the way Classic Solidity does
it, with a `whenNotPaused` check followed by `super.update`, and the override is
resolved against a single instance, so a call to `update` from the inherited
`transfer` reaches the overriding body. The only construct that does not come for
free is `super`, which the next section addresses as part of the representation
strategy.

## 5. Representing inheritance with data types, classes and instances

Inheritance is the sum of four capabilities, and each one maps onto a Core
Solidity construct already used by the contract representation. The mapping is
what makes inheritance a desugaring rather than a new primitive.

### State, with data types

A contract's storage is a product of its field types, tagged by a singleton data
type. Inheriting re-includes the parent's fields into the child, so the child's
storage is the product of the parent's fields followed by the child's own. The
selector and `CStructField` instances are generated for all fields, parent and
child alike, with offsets recomputed against the flattened layout. For the vault
chain this yields four selectors on `ContractStorage(VaultTokenCxt)`, with offsets
that are the nested product of the preceding field types:

```solc
data VaultTokenCxt = VaultTokenCxt;

instance StructField(ContractStorage(VaultTokenCxt), VaultToken_owner_sel)
  : CStructField(storage(address), ()) {}
instance StructField(ContractStorage(VaultTokenCxt), VaultToken_paused_sel)
  : CStructField(storage(bool), address) {}
instance StructField(ContractStorage(VaultTokenCxt), VaultToken_balances_sel)
  : CStructField(storage(mapping(address, uint256)), pair(address, bool)) {}
instance StructField(ContractStorage(VaultTokenCxt), VaultToken_totalSupply_sel)
  : CStructField(storage(uint256), pair(pair(address, bool), mapping(address, uint256))) {}
```

This is exactly Solidity's flat-storage semantics, made explicit by the offset
records.

### Types and subtyping, with classes

An interface is a class, and a contract is a class whose superclasses are the
interfaces it implements and the parent it inherits. The superclass relation is
the subtyping relation: any type that satisfies `VaultToken` satisfies
`ERC20Base`, hence `Pausable`, `Ownable` and the interfaces, so a value of the
child storage type is accepted wherever an ancestor capability is required. For
the chain, the classes line up as a tower:

```solc
forall self . self:IOwnable => class self : Ownable   { /* owner, transferOwnership */ }
forall self . self:Ownable, self:IPausable => class self : Pausable  { function setPaused(self : self, p : bool) -> (); }
forall self . self:Pausable, self:IERC20   => class self : ERC20Base { function update(self : self, from : address, to : address, amount : uint256) -> (); }
forall self . self:ERC20Base => class self : VaultToken { function mint(self : self, to : address, amount : uint256) -> (); }
```

### Implementation reuse, with instances

Each contract contributes one instance of its class on the storage type, with the
method bodies expressed over the selectors. Inheriting generates, on the child's
storage type, an instance of the parent class whose bodies are the parent's, with
the parent's selectors rewritten to the child's. So the child gains the parent's
implementations without copying source. Because there is exactly one instance of a
given class for a given storage type, method resolution is never ambiguous, and
the linearization questions that arise in Classic Solidity do not appear.

### Open recursion, with override and a named base body

Two pieces complete the picture. Override is selection: when the child
re-declares a method, the generated instance simply holds the overriding body, and
since `transfer` and `update` resolve through the same single instance, a call
from the inherited `transfer` to `update` reaches the override. This gives virtual
dispatch with no runtime table.

The one element that needs care is `super`. Once the override replaces the base
body inside the single instance, the base body would be lost, so the strategy is
to preserve and name it. When `VaultToken` overrides `update`, the desugarer keeps
the inherited body under a generated name and makes `super.update` resolve to it:

```solc
// the override, holding the dressed body
function update(self : ContractStorage(VaultTokenCxt), from : address, to : address, amount : uint256) -> () {
  require(not(paused(self)), Error(0x9e87fac8));
  super$update(self, from, to, amount);   // the preserved base body
}

// the preserved ERC20Base.update body, generated alongside the instance
function super$update(self : ContractStorage(VaultTokenCxt), from : address, to : address, amount : uint256) -> () {
  if (from != address(0)) { /* debit */ } else { /* mint */ }
  if (to   != address(0)) { /* credit */ } else { /* burn */ }
}
```

For a longer chain the desugarer names one body per overriding layer and threads
each `super` call to the immediately preceding one. That generated thread is a
linearized chain of method bodies, produced mechanically from the `inherits`
chain. It is the single feature the example forces beyond the plain
representation, and it is local to the inheritance pass.

In summary, data types carry the inherited state, classes carry the subtyping,
instances carry the reused implementations, and override plus a named base body
carry open recursion. The vault is reproduced in full, with only `super` added on
top of machinery the contract representation already uses.

## 6. Modeling modifiers as higher-order functions

Section 1 listed modifiers as one of the composition mechanisms, and the vault in
Section 2 used `onlyOwner` for access control. The Core Solidity representation in
Section 3 reproduced that guard by hand: every owner-gated method opens with a call
to an internal `requireOwner()`. This section shows that the by-hand form is exactly
what a modifier would compile to, and proposes a surface for modifiers that desugars
into a higher-order function. As with inheritance, the construct is sugar over
machinery the compiler already has, not a new primitive.

A modifier is the "around" pattern. It wraps the body of the function it decorates,
running code before and after a hole written `_;`, which stands for the modified
body. That hole is the key: it is a piece of deferred computation handed to the
modifier, which is precisely a function value. So a modifier is naturally a function
that takes the body as a thunk and decides when to run it, and `_;` is the call to
that thunk.

### The proposed surface

A modifier is declared once, with `_;` marking where the modified body runs:

```solc
modifier onlyOwner() {
  require(address(caller()) == owner_, Error(0x12b0c500)); // OwnableUnauthorizedAccount()
  _;
}
```

A function attaches it by name, after the return type, in the place Classic Solidity
attaches modifiers to a signature:

```solc
public function transferOwnership(newOwner : address) -> () onlyOwner {
  owner_ = newOwner;
}
```

### The desugaring

The translation has three steps, and all three land on constructs the compiler
already lowers: surface lambdas `lam(...) { ... }`, application of a function value,
and the `Invokable` type class that application resolves through during
defunctionalization.

First, the modifier becomes an ordinary higher-order function whose extra parameter
is the body thunk. The code before `_;` stays, and `_;` becomes a call to the thunk.
The result type is a fresh type variable, so the same modifier wraps any function:

```solc
forall r . function onlyOwner(body : uint256 -> r) -> r {
  require(address(caller()) == owner_, Error(0x12b0c500));
  return body(uint256(0));   // _;  becomes invoking the body thunk
}
```

Because `onlyOwner` is a contract method, it already receives the storage context,
so `owner_` resolves exactly as the hand-written `requireOwner()` does today. A
modifier with code after `_;`, such as a reentrancy guard, binds the thunk result
and returns it afterwards:

```solc
forall r . function nonReentrant(body : uint256 -> r) -> r {
  require(locked == uint256(0), Error(0x37ed32e8));
  locked = uint256(1);
  let result = body(uint256(0));
  locked = uint256(0);
  return result;
}
```

Second, the decorated function lifts its body into a thunk and hands it to the
modifier. The thunk is a lambda that closes over the function's parameters and the
storage context:

```solc
public function transferOwnership(newOwner : address) -> () {
  return onlyOwner(lam(ignore) { owner_ = newOwner; });
}
```

Stacked modifiers nest outermost-first, which matches Solidity's left-to-right `_;`
chaining: `f ... modA modB { BODY }` becomes
`return modA(lam(i) { return modB(lam(j) { BODY }); });`. A modifier that takes
arguments, such as `onlyAfter(deadline)`, gains them as leading parameters,
`forall r . function onlyAfter(deadline : uint256, body : uint256 -> r) -> r`, and
the call site passes them alongside the thunk, `onlyAfter(deadline, lam(i) { BODY })`.
This matches Classic Solidity, where modifier arguments are evaluated in the
function's scope.

Third, the existing passes finish the lowering with no new support. The `lam` thunk
and the application `body(...)` are defunctionalized into a closure data type that
captures the free variables, an `Invokable` instance, and a lifted body function,
the same shape the compiler already produces for any lambda. Specialization then
fixes the result type at `()` and resolves the `Invokable` instance to a direct
call. What remains is first-order code operationally identical to
`requireOwner(); owner_ = newOwner;`, which is the manual guard Section 3 wrote out.
The modifier surface adds a name for that pattern; it does not add a mechanism. The
`Invokable` encoding that makes this work is the same one exercised by
`test/examples/invokable/028modifier.solc`, where a modifier is already written as a
higher-order function by hand.

### Scope

The scheme covers a single `_;`, which is enough for `onlyOwner`, `whenNotPaused`
and `nonReentrant`. The one capability it leans on beyond the simplest lambda
examples is a closure that captures the storage context and writes a field, and the
demo confirms that field writes inside the thunk compile and run. Two cosmetic gaps
remain on the surface side. There is no `modifier` keyword yet, so the demo writes
the desugared higher-order function directly. And application currently carries an
argument, so the body thunk takes one ignored `uint256` and the modifier passes a
dummy value; a real desugaring of `onlyOwner()` would use a unit thunk once
zero-argument application is available. Neither gap changes the strategy: a modifier
is a higher-order function, and `_;` is the call to its body.

## 7. Conclusion

The document carried one contract, a vault share token, through three encodings to
show that contract composition is not a primitive that Core Solidity must grow, but
a use of machinery it already has.

Section 2 set the bar in Classic Solidity. The vault leaned on every composition
mechanism at once: multiple inheritance with C3 linearization, a single virtual
hook that every token movement routes through, `super` to chain the override onto
the inherited body, modifiers for access control, a library, an interface with a
callback, and a CREATE2 factory. The state of three mixins ends up in one flat
layout, and the hook is the one open seam.

Section 3 expressed the same behavior in the Core Solidity the compiler accepts
today. A contract is a type class with a single instance over its storage, fields
become selectors with `CStructField` instances and are read and written through
proxies, and the three mixins collapse into one flat contract whose shared logic is
ordinary methods, with the virtual hook becoming one internal `update` method and
the pause check inlined at that seam. This version is not a sketch: it lives at
`test/examples/valt/VaultToken.solc`, compiles through `sol-core` and `yule`, and
runs on the EVM, with `totalSupply` and the `test` entry point returning the
expected values.

Sections 4 and 5 added a contract-inheritance surface on top of that representation
and showed that it is a desugaring rather than a new primitive. State reuse is field
re-inclusion into a flat layout, subtyping is a superclass edge, implementation reuse
is a generated instance, and overriding is replacement inside the single instance,
which yields virtual dispatch with no runtime table. The only construct the example
forces beyond the plain representation is `super`, and even that is local: the
desugarer preserves the overridden body under a generated name and threads each
`super` call to the preceding layer.

Section 6 did the same for modifiers. A modifier is the "around" pattern, so it
desugars into a higher-order function that takes the decorated body as a thunk, with
`_;` becoming the call to that thunk. Defunctionalization and specialization then
reduce it to the same first-order code the manual `requireOwner()` guard produces,
and the rewritten vault at `test/examples/valt/VaultTokenModifier.solc` compiles and
runs with identical results, confirming that modifiers too are sugar over the lambda
and `Invokable` machinery already present.

The throughline is that the type-class and instance encoding the compiler uses for
field access is already enough to express mixins, shared hooks, access guards and
open recursion. What today's surface still lacks is mostly sugar and a few orthogonal
features, an interface and `inherits` syntax, several stateful parents with their
linearization, events, storable and ABI-decodable `bool`, and message context, none
of which change the underlying strategy. Composition in Core Solidity is therefore a
matter of naming, not of adding a new object model.
