// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

// Reference Classic-Solidity token: the differential oracle for BOTH Core Solidity
// translations (oz/ERC20Inherit.solc and oz/ERC20Flat.solc). Same ABI and behavior:
// OpenZeppelin ERC20 + ERC20Pausable + ERC20Capped (a real diamond over ERC20),
// with mint / burn / burnFrom / pause / unpause. No constructor mint — the cap is
// set first, then the initial supply is minted by an explicit mint() call.
//
// differential.sh compiles this with solc and runs the exact same vector sequence
// (oz/ERC20Inherit.json) that both Core versions run, checking all three produce
// byte-identical returndata and status.
//
// `contracts/` imports resolve via a solc remapping `contracts/=<oz>/contracts/`.

import {ERC20} from "contracts/token/ERC20/ERC20.sol";
import {ERC20Pausable} from "contracts/token/ERC20/extensions/ERC20Pausable.sol";
import {ERC20Capped} from "contracts/token/ERC20/extensions/ERC20Capped.sol";

contract Token is ERC20, ERC20Pausable, ERC20Capped {
    constructor(string memory n, string memory s, uint256 cap_)
        ERC20(n, s)
        ERC20Capped(cap_)
    {}

    function pause() external { _pause(); }
    function unpause() external { _unpause(); }
    function mint(address to, uint256 v) external { _mint(to, v); }
    function burn(uint256 v) external { _burn(msg.sender, v); }
    function burnFrom(address a, uint256 v) external { _spendAllowance(a, msg.sender, v); _burn(a, v); }

    // Diamond: ERC20, ERC20Pausable and ERC20Capped all define _update.
    function _update(address from, address to, uint256 value)
        internal
        override(ERC20, ERC20Pausable, ERC20Capped)
    {
        super._update(from, to, value);
    }
}
