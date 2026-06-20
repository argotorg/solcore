import std.{*};
import std.opcodes.{caller as caller_, callvalue as callvalue_, selfbalance, gas, call};
import std.dispatch.{*};

// Forward `wad` wei to `dst` via a zero-data CALL and revert on failure.
function sendValue(dst: address, wad: uint256) -> () {
    let ret = call(gas(), Typedef.rep(dst), Typedef.rep(wad), 0, 0, 0, 0);
    require(ret != 0, Error(0x90b8ec18)); // TransferFailed()
}

function caller() -> address {
    return address(caller_());
}

function callvalue() -> uint256 {
    return uint256(callvalue_());
}

// Based on https://github.com/gnosis/canonical-weth/blob/master/contracts/WETH9.sol
// That code is written WITHOUT checked arithmetic.
contract WETH9 {
    balances : mapping(address, uint256);
    allowance : mapping(address, mapping(address, uint256));

    constructor() {}

    // --- ETH <-> WETH ---

    public payable function deposit() -> () {
        let sender = caller();
        balances[sender] = balances[sender] + callvalue();
    }

    public function withdraw(wad: uint256) -> () {
        let sender = caller();
        require(balances[sender] >= wad, Error(0xf4d678b8)); // InsufficientBalance()
        balances[sender] = balances[sender] - wad;
        sendValue(sender, wad);
    }

    // totalSupply == ETH held by this contract (matches canonical WETH9).
    public function totalSupply() -> uint256 {
        return uint256(selfbalance());
    }

    // --- ERC20 surface ---

    public function balanceOf(account: address) -> uint256 {
        return balances[account];
    }

    public function allowance(owner_: address, spender: address) -> uint256 {
        return allowance[owner_][spender];
    }

    public function approve(usr: address, wad: uint256) -> bool {
        let sender = caller();
        allowance[sender][usr] = wad;
        return true;
    }

    public function transfer(dst: address, wad: uint256) -> bool {
        return transferFrom(caller(), dst, wad);
    }

    public function transferFrom(src: address, dst: address, wad: uint256) -> bool {
        let sender = caller();
        require(balances[src] >= wad, Error(0xf4d678b8)); // InsufficientBalance()

        if (src != sender && allowance[src][sender] != (maxVal():uint256)) {
            require(allowance[src][sender] >= wad, Error(0x13be252b)); // InsufficientAllowance()
            allowance[src][sender] -= wad;
        }
        balances[src] = balances[src] - wad;
        balances[dst] = balances[dst] + wad;
        return true;
    }

    // Plain ETH transfers (no calldata, just value) auto-wrap into WETH.
    payable fallback() -> () {
        let sender = caller();
        balances[sender] = balances[sender] + callvalue();
    }
}
