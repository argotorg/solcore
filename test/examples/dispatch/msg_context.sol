import * from std;
import * from std.dispatch;

contract MsgContext {
    deployer : address;

    constructor() {
        deployer = msgSender();
    }

    function sender() public returns (address) {
        return msgSender();
    }

    function deployer() public returns (address) {
        return deployer;
    }

    function value() public payable returns (uint256) {
        return msgValue();
    }
}
