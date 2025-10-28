// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract PaymentHandler {
    enum PaymentType { NATIVE, ERC20, ERC721 }
    
    struct Payment {
        PaymentType paymentType;
        address token;
        address from;
        address to;
        uint256 amount;
        uint256 tokenId;
    }
    
    function processPayment(Payment calldata payment) external {
        if (payment.paymentType == PaymentType.NATIVE) {
            require(payment.token == address(0), "Native: no token");
            require(payment.amount > 0, "Native: amount required");
            require(payment.tokenId == 0, "Native: no tokenId");
            payable(payment.to).transfer(payment.amount);
        } else if (payment.paymentType == PaymentType.ERC20) {
            require(payment.token != address(0), "ERC20: token required");
            require(payment.amount > 0, "ERC20: amount required");
            require(payment.tokenId == 0, "ERC20: no tokenId");
            IERC20(payment.token).transferFrom(payment.from, payment.to, payment.amount);
        } else if (payment.paymentType == PaymentType.ERC721) {
            require(payment.token != address(0), "ERC721: token required");
            require(payment.amount == 1, "ERC721: amount must be 1");
            require(payment.tokenId > 0, "ERC721: tokenId required");
            IERC721(payment.token).transferFrom(payment.from, payment.to, payment.tokenId);
        }
    }
    
    function calculateFee(Payment calldata payment) external pure returns (uint256) {
        if (payment.paymentType == PaymentType.NATIVE) {
            return payment.amount / 20;
        } else if (payment.paymentType == PaymentType.ERC20) {
            return payment.amount / 10;
        } else if (payment.paymentType == PaymentType.ERC721) {
            return 0.01 ether;
        }
        return 0;
    }
}
