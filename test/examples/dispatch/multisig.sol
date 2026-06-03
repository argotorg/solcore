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
// The second layer is queueWithSignature/approveWithSignature/rejectWithSignature,
// where a signature is passed along and thus the caller is not checked. This
// signature can be multiple options:
// - EIP-2098 compact ECDSA signature,
// - approved hash by target contract, which must be a signer,
// - EIP-1271 contract signature validation, which must be a signer.
//
// The last layer is batching operations.
//
// Optional future improvements:
// - EIP-712 for signing
// - Operation.ChangeSigner -- batched change to replace a given signer
// - Operation.DelegateCall -- it is a security surface, and not neccessarily needed
// - be an EIP-1271 signer
// - gas optimisations
// - Strict sequence vs. re-entracy guard for execute()

data Operation =
      AddSigner(address) // Adds a new signer.
    | RemoveSigner(address) // Removes an existing signer.
    | ChangeSigRequired(uint256) // Change the number of signatures required.
    | TransferEth(address, uint256) // Transfers ether.
    | TransferToken(address, address, uint256) // Transfers a token.
    | Call(address, uint256, memory(bytes)) // Arbitrary calls to an address.
    | UnstoredCall(bytes32); // Arbitrary calls to an address, represented by a hash (supplied at execution time).
                             // It is encoded as [address][value][payload]

data OperationStatus =
      Pending(uint256) // approval count (TODO: use uint8/uint16 to be realistic)
    | Approved
    | Rejected
    | Executed;

data Vote =
      None
    | Approved
    | Rejected;

data Signature =
      ECDSA(bytes32, bytes32) // EIP-2098-style r/s/v (TODO: add chainid/domain)
    | Contract(address) // If the hash is approved by the contract.
    | EIP1271(address, memory(bytes)); // EIP-1271 signature validation

data BatchOperation =
      Queue(Operation, Signature)
    | Approve(uint256, Signature)
    | Reject(uint256, Signature)
    | Execute(uint256, memory(bytes));

contract Multisig {
    signers: mapping(uint256 -> address); // TODO use array()
    signers_count: uint256;
    signers_required: uint256;
    // TODO: Stored by hash -- or should it be by nonce?
    operations: mapping(uint256 -> Operation); // TODO: use array()
    operations_count: uint256;
    votes: mapping(uint256 -> address -> Vote);
    status: mapping(uint256 -> OperationStatus);
    nonce: uint256; // Strict ordering. Next executable operation.

    constructor() -> () {
        // The creator becomes the first signer.
        signers[0] = caller();
        signers_count = 1;
        signers_required = 1;
    }

    // Only signers can call this.
    function queue(op: Operation) -> () {
        require(isSigner(caller()), Error(0x12345678)); // NotASigner()
        perform_queue(op);
    }

    // TODO: mark private
    function perform_queue(op: Operation) -> () {
        // Some basic sanity checks.
        match op {
            | AddSigner(signer) =>
                require(signer != address(0), Error(0x12345678)); // CannotAddZeroAddressAsSigner()
                require(signer != address(this), Error(0x12345678)); // CannotAddSelfAsSigner()
            | ChangeSigRequired(count) =>
                require(count >= 1, Error(0x12345678)); // ThresholdBelowMinimum()
        }

        operations[operations_count] = op;
        status[operations_count] = OperationStatus.Pending(0);
        operations_count += 1;

        // TODO: emit log
    }

    // Only signers can call this.
    function approve(nonce_: uint256) -> () {
        require(isSigner(caller()), Error(0x12345678)); // NotASigner()
        perform_approve(nonce_, caller());
    }

    // TODO: mark private
    function perform_approve(nonce_: uint256, signer: address) -> () {
        require(nonce_ < operations_count, Error(0x12345678)); // OperationNotFound()

        // TODO: emit log

        match status[nonce_] {
            | Pending(count) =>
                require(votes[nonce_][signer] == Vote.None, Error(0x12345678)); // SignerAlreadyApproved()
                votes[nonce_][signer] = Vote.Approved;

                if (count + 1 >= signers_required) {
                    status[nonce_] = OperationStatus.Approved;
                } else {
                    status[nonce_] = OperationStatus.Pending(count + 1);
                }
            | _ => revertWithError(Error(0x12345678)); // UnexpectedStatus()
        }
    }


    function checkSignature(hash: bytes32, signature: Signature) -> address {
        match signature {
            | ECDSA(r, s) =>
                let signer = eip2098_signer(hash, r, s);
                require(isSigner(signer), Error(0x12345678)); // NotASigner()
                return signer;
            | Contract(contract) =>
                require(isSigner(contract), Error(0x12345678)); // NotASigner()
                require(check_contract_hash(contract, hash), Error(0x12345678)); // HashNotApprovedByTarget()
                return contract;
            | EIP1271(contract, signature) =>
                require(isSigner(contract), Error(0x12345678)); // NotASigner()
                require(eip1271_verify(contract, hash, signature), Error(0x12345678)); // EIP1271VerificationRejected()
                return contract;
        }
    }

    // Anyone can call this.
    function queueWithSignature(operation: Operation, signature: Signature) -> () {
        // TODO: include domain/chaind information in hash
        let hash = abi_encode(operation);

        checkSignature(hash, signature);

        perform_queue(operation);
    }

    // Anyone can call this.
    function approveWithSignature(nonce_: uint256, signature: Signature) -> () {
        // TODO: include domain/chaind information in hash
        let hash = abi_encode(operations[nonce_]);

        let signer = checkSignature(hash, signature);

        perform_approve(nonce_, signer);
    }

    // Anyone can call this.
    function rejectWithSignature(nonce_: uint256, signature: Signature) -> () {
        // TODO: include domain/chaind information in hash
        let hash = abi_encode(operations[nonce_]);

        let signer = checkSignature(hash, signature);

        perform_reject(nonce_, signer);
    }

    function batch(operations: array(BatchOperation)) -> () {
        for (let i = 0; i < operations.length; i++) {
            match operations[i] {
                | Queue(operation, signature) => queueWithSignature(operation, signature);
                | Approve(nonce_, signature) => approveWithSignature(nonce_, signature);
                | Reject(nonce_, signature) => rejectWithSignature(nonce_, signature);
                | Execute(nonce_, payload) => execute(nonce_, payload);
            }
        }
    }

    // Only signers can call this.
    function reject(nonce_: uint256) -> () {
        require(isSigner(caller()), Error(0x12345678)); // NotASigner()
        perform_reject(nonce_, caller());
    }

    // TODO: mark private
    function perform_reject(nonce_: uint256, signer: address) -> () {
        require(nonce_ < operations_count, Error(0x12345678)); // OperationNotFound()

        // TODO: emit log

        match status[nonce_] {
            | Pending(count) =>
                status[nonce_] = OperationStatus.Rejected;
                votes[nonce_][signer] = Vote.Rejected;
            | Approved =>
                status[nonce_] = OperationStatus.Rejected;
                votes[nonce_][signer] = Vote.Rejected;
            | _ => revertWithError(Error(0x12345678)); // UnexpectedStatus()
        }
    }

    // Anyone can execute, as long as the status is correct.
    // Payload is optional, used in case UnstoredCall is encountered.
    function execute(nonce_: uint256, payload: memory(bytes)) -> () {
        // Ensure status.
        require(nonce_ < operations_count, Error(0x12345678)); // OperationNotFound()

        // Enforce strict sequence ordering.
        require(nonce_ == nonce, Error(0x12345678)); // IncorrectSequence()
        match status[nonce_] {
            | Rejected =>
                nonce += 1;
                // Special case for rejections: we operate as a no-op.
                return;
            | Approved =>
                nonce += 1;
                // Update status.
                status[nonce_] = OperationStatus.Executed;
            | _ => revertWithError(Error(0x12345678)); // IncorrectStatus();
        }

        // TODO: emit log

        // Execute.
        match operations[nonce_] {
            | AddSigner(signer) => add_signer(signer);
            | RemoveSigner(signer) => remove_signer(signer);
            | ChangeSigRequired(count) =>
                require(count <= signers_count, Error(0x12345678)); // ThresholdExceedsSigners()
                signers_required = count;
            | TransferEth(target, amount) =>
                let ret: word;
                assembly {
                    ret := call(gas(), target, amount, 0, 0, 0, 0)
                }
                require(tobool(ret), Error(0x12345678)); // EtherTransferFailed()
            | TransferToken(target, token, amount) =>
                safe_erc20_transfer(token, target, amount);
            | UnstoredCall(hash) =>
                require(hash == keccak256(payload), Error(0x12345678)); // InvalidPayloadSupplied()
                let ret: word;
                let payload_ = Typedef.rep(payload);
                assembly {
                    let size := mload(payload_)
                    // Check for minimum length of 64 bytes
                    if lt(size, 64) {
                        revert(0, 0) // TODO return proper error
                    }
                    let target := mload(add(payload_, 32))
                    let value := mload(add(payload_, 64))
                    ret := call(gas(), target, value, add(payload_, 96), sub(size, 64), 0, 0)
                }
                require(tobool(ret), Error(0x12345678)); // UnstoredCallFailed()
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

function check_contract_hash(contract: address, hash: bytes32) -> bool {
    let ptr = get_free_memory();
    let contract_ = Typedef.rep(contract);
    let hash_ = Typedef.rep(hash);
    let res: word;
    // We assume the [0, 32] scratch space is reserved.
    // TODO: add specific error code
    assembly {
        mstore(ptr, shl(224, 0x12345678)) // IsHashApproved(bytes32)
        mstore(add(ptr, 4), hash_)
        // Alternative option is ignoring ret, but setting mem[0] to 0.
        let ret := staticcall(gas(), contract_, ptr, 36, 0, 32)
        res := mload(0)
    }
    return ret == 1 && res == 0x12345678; // Must match the magic.
}

function eip1271_verify(contract: address, hash: bytes32, signature: memory(bytes)) -> bool {
    let ptr = get_free_memory();
    let contract_ = Typedef.rep(contract);
    let hash_ = Typedef.rep(hash);
    let signature_ = Typedef.rep(signature);
    let res: word;
    // We assume the [0, 32] scratch space is reserved.
    // TODO: add specific error code
    assembly {
        // TODO: use abi.encode to build this
        mstore(ptr, shl(224, 0x1626ba7e))
        mstore(add(ptr, 4), hash_)
        mstore(add(ptr, 36), 64)
        let size := mload(signature_)
        mstore(add(ptr, 68), size)
        mcopy(add(ptr, 100), add(signature_, 32), size)
        // Alternative option is ignoring ret, but setting mem[0] to 0.
        let ret := staticcall(gas(), contract_, ptr, add(100, size), 0, 32)
        res := mload(0)
    }
    return ret == 1 && res == 0x1626ba7e; // Must match the magic.
}

function eip2098_signer(hash: bytes32, r: bytes32, s_: bytes32) -> address {
    let s: word;
    let v: word;
    assembly {
        s := and(s_, sub(shl(255, 1), 1))
        v := add(shr(255, s_), 27)
    }
    // TODO: enforce s ≤ secp256k1n/2
    let parity = match v {
        | 27 => Even,
        | 28 => Odd,
    }
    return ecrecover(hash, uint256(v), r, bytes32(s));
}

data ECDSAParity = Even | Odd;

// TODO: use uint8
function ecrecover(hash: bytes32, v: uint256, r: bytes32, s: bytes32) -> address {
    let hash_ = Typedef.rep(hash);
    let v_ = Typedef.rep(v);
    let r_ = Typedef.rep(r);
    let s_ = Typedef.rep(s);
    let ptr = get_free_memory();
    let res: word;
    // We assume the [0, 32] scratch space is reserved.
    // TODO: add specific error code
    assembly {
        mstore(ptr, hash_)
        mstore(add(ptr, 32), v_)
        mstore(add(ptr, 64), r_)
        mstore(add(ptr, 96), s_)

        let ret := staticcall(gas(), 1, ptr, 128, 0, 32)
        if iszero(ret) {
            revert(0, 0)
        }
        res := mload(0)
        if iszero(res) {
            revert(0, 0)
        }
    }
    return address(res);
}

// Performs a safe transfer of ERC-20 tokens. Makes sure the call succeeded,
// and if the token follows the standard and returns a boolean, that is also true.
function safe_erc20_transfer(token: address, to: address, value: uint256) -> () {
    let ptr = get_free_memory();
    let token_ = Typedef.rep(token);
    let to_ = Typedef.rep(to);
    let value_ = Typedef.rep(value);
    assembly {
        // Assemble the [selector][address][value]
        mstore(ptr, shl(224, 0xa9059cbb))
        mstore(add(ptr, 4), to_)
        mstore(add(ptr, 36), value_)
        let ret := call(gas(), token_, 0, ptr, 68, 0, 32)
        if iszero(ret) {
            // Bubble up error.
            returndatacopy(0, 0, returndatasize())
            revert(0, returndatasize())
        }
        // If the token follows the standard and returns a bool, check it returned true.
        // This allows any non-zero value as true, just like OpenZeppelin.
        if returndatasize() {
            if iszero(mload(0)) {
                revert(0, 0) // TODO: use error codes
            }
        }
    }
}
