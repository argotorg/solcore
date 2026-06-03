//
// The design of this multisig is fairly simple.
//
// An Operation defines a state change, and OperationStatus defines
// its current status.  Each Operation must be approved by enough Signers.
// We store Signers, Operations and OperationStatuses in storage.
//
// An existing Signer can queue, approve, or reject an Operation. Once
// an Operation is approved, anyone can execute it. OperationStatus controls
// it as a state machine.
//
// The states of an Operation:
// - upon creation, called `queue`, the state becomes Pending(0), where 0 means 0 approvals
// - with `approve` the state increments Pending(i) to Pending (i + 1) or Approved iff i + 1 == signers_required
// - with `reject` the state changes to Rejected iff the current state is Pending(i) or Approved
// - with `execute` the state changes to Executed iff the current state is Approved
//
// Optional future improvements:
// - Passing signatures with operations
// - Batching
// - EIP-712 for signing
// - Operation.ChangeSigner -- batched change to replace a given signer
// - Operation.DelegateCall -- it is a security surface, and not neccessarily needed
// - be an EIP-1271 signer
// - gas optimisations

data Operation =
      AddSigner(address) // Adds a new signer.
    | RemoveSigner(address) // Removes an existing signer.
    | ChangeSigRequired(uint256) // Change the number of signatures required.
    | TransferEth(address, uint256) // Transfers ether.
    | TransferToken(address, address, uint256) // Transfers a token.
    | Call(address, uint256, memory(bytes)) // Arbitrary calls to an address.
    | UnstoredCall(address, bytes32); // Arbitrary calls to an address, represented by a hash (supplied at execution time).

data OperationStatus =
      Pending(uint256) // approval count (TODO: use uint8/uint16 to be realistic)
    | Approved
    | Rejected
    | Executed;

contract Multisig {
    signers: mapping(uint256 -> address); // TODO use array()
    signers_count: uint256;
    signers_required: uint256;
    // TODO: Stored by hash -- or should it be by nonce?
    operations: mapping(uint256 -> Operation); // TODO: use array()
    operations_count: uint256;
    nonce: uint256; // current nonce
    status: mapping(uint256 -> OperationStatus);

    constructor() -> () {
        // The creator becomes the first signer.
        signers[0] = caller();
        signers_count = 1;
        signers_required = 1;
    }

    // Only signers can call this.
    function queue(op: Operation) -> () {
        require(isSigner(caller()), Error(0x12345678)); // NotASigner()

        operations[operations_count] = op;
        status[operations_count] = OperationStatus.Pending(0);
        operations_count += 1;

        // TODO: emit log
    }

    // Only signers can call this.
    function approve(nonce_: uint256) -> () {
        require(isSigner(caller()), Error(0x12345678)); // NotASigner()
        require(nonce_ < operations_count, Error(0x12345678)); // OperationNotFound()

        // TODO: emit log

        match status[nonce_] {
            | Pending(count) =>
                if (count + 1 >= signers_required) {
                    status[nonce_] = OperationStatus.Approved;
                } else {
                    status[nonce_] = OperationStatus.Pending(count + 1);
                }
            | _ => revertWithError(Error(0x12345678)); // UnexpectedStatus()
        }
    }

    // Only signers can call this.
    function reject(nonce_: uint256) -> () {
        require(isSigner(caller()), Error(0x12345678)); // NotASigner()
        require(nonce_ < operations_count, Error(0x12345678)); // OperationNotFound()

        // TODO: emit log

        match status[nonce_] {
            | Pending(count) =>
                status[nonce_] = OperationStatus.Rejected;
            | Approved =>
                status[nonce_] = OperationStatus.Rejected;
            | _ => revertWithError(Error(0x12345678)); // UnexpectedStatus()
        }
    }

    // Anyone can execute, as long as the status is correct.
    function execute(nonce_: uint256) -> () {
        // Ensure status.
        require(nonce_ < operations_count, Error(0x12345678)); // OperationNotFound()
        require(nonce_ == nonce, Error(0x12345678)); // IncorrectSequence()
        require(status[nonce_] == OperationStatus.Approved, Error(0x12345678)); // IncorrectStatus()

        // Update status.
        status[nonce_] = OperationStatus.Executed;
        nonce += 1;

        // TODO: emit log

        // Execute.
        match operations[nonce_] {
            | AddSigner(signer) => add_signer(signer);
            | RemoveSigner(signer) => remove_signer(signer);
            | _ => unimplemented(); // TODO
        }
    }

    payable fallback() -> () {
        // Accept incoming payments unconditionally.
    }

    // TODO: these functions should be non-public

    // TODO: this is suboptimal
    function isSigner(signer: address) -> bool {
        for (let i = 0; i < signers_count; i++) {
            if (signers[i] == signer) {
                return true;
            }
        }
        return false;
    }

    function add_signer(signer: address) -> () {
        require(!isSigner(signer), Error(0x12345678)); // SignerAlreadyExists()
        signers[signers_count] = signer;
        signers_count += 1;
    }

    function remove_signer(signer: address) -> () {
        require(signers_count > 1, Error(0x12345678)); // CannotRemoveOnlySigner()
        for (let i = 0; i < signers_count; i++) {
            if (signers[i] == signer) {
                // Move last signer into this place.
                signers[i] = signers[signers_count - 1];
                signers_count -= 1;
                // Reduce requirement if needed.
                if (signers_count < signers_required) {
                    signers_required = signers_count;
                }
                return;
            }
        }
        revertWithError(Error(0x12345678)); // NotASigner()
    }
}

function caller() -> address {
    let ret;
    assembly {
        ret := caller()
    }
    return address(ret);
}