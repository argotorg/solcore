// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract Fib {
    constructor() {}

    function fib(uint256 n) internal pure returns (uint256) {
        if (n < 2) {
            return n;
        } else {
            return fib(n - 1) + fib(n - 2);
        }
    }

    function test() public pure returns (uint256) {
        return fib(10);
    }
}
