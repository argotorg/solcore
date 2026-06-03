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
    approvals: mapping(uint256 -> address -> bool);
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

        // Some basic sanity checks.
        match op {
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
        require(nonce_ < operations_count, Error(0x12345678)); // OperationNotFound()

        // TODO: emit log

        match status[nonce_] {
            | Pending(count) =>
                require(!approvals[nonce_][caller()], Error(0x12345678)); // SignerAlreadyApproved()
                approvals[nonce_][caller()] = true;

                if (count + 1 >= signers_required) {
                    status[nonce_] = OperationStatus.Approved;
                } else {
                    status[nonce_] = OperationStatus.Pending(count + 1);
                }
            | _ => revertWithError(Error(0x12345678)); // UnexpectedStatus()
        }
    }


    function checkSignature(hash: bytes32, signature: Signature) -> () {
        match signature {
            | ECDSA(r, s) =>
                let signer = eip2098_signer(hash, r, s);
                require(isSigner(signer), Error(0x12345678)); // NotASigner()
            | Contract(contract) =>
                require(isSigner(contract), Error(0x12345678)); // NotASigner()
                require(check_contract_hash(contract, hash), Error(0x12345678)); // HashNotApprovedByTarget()
            | EIP1271(contract, signature) =>
                require(isSigner(contract), Error(0x12345678)); // NotASigner()
                require(eip1271_verify(contract, hash, signature), Error(0x12345678)); // EIP1271VerificationRejected()
        }
    }

    // Anyone can call this.
    function queueWithSignature(operation: Operation, signature: Signature) -> () {
        // TODO: include domain/chaind information in hash
        let hash = abi_encode(operation);

        checkSignature(hash, signature);

        // TODO: implement
        unimplemented();
    }

    // Anyone can call this.
    function approveWithSignature(nonce_: uint256, signature: Signature) -> () {
        // TODO: include domain/chaind information in hash
        let hash = abi_encode(operations[nonce_]);

        checkSignature(hash, signature);

        // TODO: implement
        unimplemented();
    }

    // Anyone can call this.
    function rejectWithSignature(nonce_: uint256, signature: Signature) -> () {
        // TODO: include domain/chaind information in hash
        let hash = abi_encode(operations[nonce_]);

        checkSignature(hash, signature);

        // TOOD: implement
        unimplemented();
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
    // Payload is optional, used in case UnstoredCall is encountered.
    function execute(nonce_: uint256, payload: memory(bytes)) -> () {
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
            | ChangeSigRequired(count) =>
                require(count <= signers_count, Error(0x12345678)); // ThresholdExceedsSigners()
                signers_required = count;
            | TransferEth(target, amount) =>
                let ret: word;
                assembly {
                    ret := call(gas(), target, amount, 0, 0, 0, 0)
                }
                require(tobool(ret), Error(0x12345678)); // EtherTransferFailed()
            | UnstoredCall(target, hash) =>
                require(hash == keccak256(payload), Error(0x12345678)); // InvalidPayloadSupplied()
                let ret: word;
                let payload_ = Typedef.rep(payload);
                assembly {
                    // TODO: split up contents as <gas | amount | data>
                    ret := call(gas(), target, 0, add(payload_, 32), mload(payload_), 0, 0)
                }
                require(tobool(ret), Error(0x12345678)); // CallFailed()
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
