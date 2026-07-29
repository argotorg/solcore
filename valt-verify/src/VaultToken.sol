// SPDX-License-Identifier: MIT
pragma solidity ^0.8.24;

// ---- a library used through `using ... for` ----
library Math {
    function addChecked(uint256 a, uint256 b) internal pure returns (uint256 c) {
        c = a + b;               // 0.8.x reverts on overflow; this only illustrates `using`
    }
    function subChecked(uint256 a, uint256 b) internal pure returns (uint256) {
        require(a >= b, "underflow");
        return a - b;
    }
}

// ---- an interface and a callback ----
interface IFlashBorrower {
    // receives freshly minted tokens and returns a magic value when it accepts them
    function onFlashMint(address initiator, uint256 amount, bytes calldata data)
        external returns (bytes32);
}

// ---- ownership mixin: its own state plus a modifier ----
abstract contract Ownable {
    address private _owner;
    error NotOwner();
    constructor() { _owner = msg.sender; }
    modifier onlyOwner() { if (msg.sender != _owner) revert NotOwner(); _; }
    function owner() public view returns (address) { return _owner; }
    function transferOwnership(address to) public onlyOwner { _owner = to; }
}

// ---- pause mixin: its own state plus a modifier that other code attaches ----
abstract contract Pausable {
    bool private _paused;
    error Paused();
    function paused() public view returns (bool) { return _paused; }
    function _setPaused(bool p) internal { _paused = p; }
    modifier whenNotPaused() { if (_paused) revert Paused(); _; }
}

// ---- ERC-20 base with a single virtual hook every movement flows through ----
abstract contract ERC20Base {
    using Math for uint256;

    mapping(address => uint256) public balanceOf;
    uint256 public totalSupply;
    event Transfer(address indexed from, address indexed to, uint256 value);

    // the one seam: mint, burn and transfer all route through it
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

// ---- final contract: composition by multiple inheritance plus a hook override ----
contract VaultToken is Ownable, Pausable, ERC20Base {
    bytes32 private constant FLASH_OK = keccak256("IFlashBorrower.onFlashMint");

    constructor(uint256 initialSupply) {
        _update(address(0), msg.sender, initialSupply); // initial mint to deployer
    }

    // Pausable dresses the hook inherited from ERC20Base
    function _update(address from, address to, uint256 amount)
        internal override whenNotPaused
    {
        super._update(from, to, amount);   // delegate to the linearized base
    }

    function setPaused(bool p) external onlyOwner { _setPaused(p); }

    function mint(address to, uint256 amount) external onlyOwner {
        _update(address(0), to, amount);
    }

    // flash-mint: mint, call the borrower back, require the magic value, burn
    function flashMint(IFlashBorrower borrower, uint256 amount, bytes calldata data)
        external
    {
        _update(address(0), address(borrower), amount);          // ephemeral mint
        bytes32 ok = borrower.onFlashMint(msg.sender, amount, data);
        require(ok == FLASH_OK, "callback failed");
        _update(address(borrower), address(0), amount);          // burn back
    }
}

// ---- factory: deterministic deploy with CREATE2 ----
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
