struct 
data S = S(Pair(Uint256, Pair(Bool, Bytes32)))

data Operation =
    AddSigner(address) // Adds a new signer.
  | RemoveSigner(address) // Removes an existing signer.
  | ChangeSigRequire(uint256) // Change the number of signatures required.
  | TransferEth(address, uint256) // Transfers ether.
  | TransferToken(address, address, uint256) // Transfers a token.
  | Call(address, uint256, memory(bytes)); // Arbitrary calls to an address.

contract Multisig {
    signers: array(address);
    nonce: uint256;

    function changeSigner

    payable fallback() -> () {
        // Accept incoming payments unconditionally.
    }
}