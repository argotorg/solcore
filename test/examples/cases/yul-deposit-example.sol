import {*} from std;

function deposit(pubkey: string memory, withdrawal_credentials: string memory, signature: string memory, deposit_data_root: uint256) returns (()) {
    let msg_value : word = 0;
    assembly {
        msg_value := callvalue()
    }
}

contract Foo {
   function main () public returns (()) {
      deposit(memory(0), memory(0), memory(0), uint256(2));
   }
}
