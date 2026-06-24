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
// - upon creation, called `queue`, the state becomes Approvals(0), where 0 means 0 approvals
// - with `approve` the state increments Approvals(i) to Approvals (i + 1)
// - with `reject` the state changes to Rejected iff the current state is Approvals(i)
// - with `execute` the state changes to Executed iff the current state is Approvals(i) with i >= signers_required
//
// Operations must be executed in strict order. If something becomes Rejected, it must
// still be executed, and execution will mark and skip it. Note that if a transaction
// becomes non-executable for any reason, it can be marked as rejected and skipped.
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

import std.{*};
import std.dispatch.{*};
import std.opcodes.{address as address_, calldatasize, mload, caller as caller_};
import std.ABIGeneric.{*};
import std.StorageGeneric.{*};

function caller() -> address {
    return address(caller_());
}

data Operation =
      AddSigner(address) // Adds a new signer.
    | RemoveSigner(address) // Removes an existing signer.
    | ChangeSigRequired(uint256) // Change the number of signatures required.
    | TransferEth(address, uint256) // Transfers ether.
    | TransferToken(address, address, uint256) // Transfers a token.
    | Call(address, uint256, memory(bytes)) // Arbitrary calls to an address.
    | UnstoredCall(bytes32)  // Arbitrary calls to an address, represented by a hash (supplied at execution time).
                             // It is encoded as [address][value][payload]
    | ApproveSignedHash(bytes32) // For interacting as an EIP-1271 signer.
    | RevokeSignedHash(bytes32);

data OperationStatus =
      Approvals(uint256) // approval count (TODO: use uint8/uint16 to be realistic)
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

data OperationKind =
      Queue
    | Approve
    | Reject;

data BatchOperation =
      Queue(Operation, Signature)
    | Approve(uint256, Signature)
    | Reject(uint256, Signature)
    | Execute(uint256, memory(bytes));


instance Vote:Eq {
  function eq(a: Vote, b: Vote) -> bool {
    let a_index: word;
    match a {
        | Vote.None => a_index = 0;
        | Vote.Approved => a_index = 1;
        | Vote.Rejected => a_index = 2;
    }
    let b_index: word;
    match b {
        | Vote.None => b_index = 0;
        | Vote.Approved => b_index = 1;
        | Vote.Rejected => b_index = 2;
    }
    return a_index == b_index;
  }
}

contract Multisig {
    signers: mapping(uint256, address); // TODO use array()
    signers_count: uint256;
    signers_required: uint256;
    // TODO: Stored by hash -- or should it be by nonce?
    operations: mapping(uint256, Operation); // TODO: use array()
    operations_count: uint256;
    votes: mapping(uint256, mapping(address, Vote));
    status: mapping(uint256, OperationStatus);
    nonce: uint256; // Strict ordering. Next executable operation.
    approved_signed_hashes: mapping(bytes32, bool);

    constructor() {
        // The creator becomes the first signer.
        signers[uint256(0)] = caller();
        signers_count = uint256(1);
        signers_required = uint256(1);
    }

    // Only signers can call this.
    public function queue(op: Operation) -> () {
        require(isSigner(caller()), Error(0x12345678)); // NotASigner()
        perform_queue(op);
    }

    function perform_queue(op: Operation) -> () {
        // Some basic sanity checks.
        match op {
            | Operation.AddSigner(signer) =>
                require(signer != address(0), Error(0x12345678)); // CannotAddZeroAddressAsSigner()
                require(signer != address(address_()), Error(0x12345678)); // CannotAddSelfAsSigner()
            | Operation.ChangeSigRequired(count) =>
                require(count >= uint256(1), Error(0x12345678)); // ThresholdBelowMinimum()
        }

        operations[operations_count] = op;
        status[operations_count] = OperationStatus.Approvals(uint256(0));
        operations_count += uint256(1);

        // TODO: emit log
    }

    // Only signers can call this.
    public function approve(nonce_: uint256) -> () {
        require(isSigner(caller()), Error(0x12345678)); // NotASigner()
        perform_approve(nonce_, caller());
    }

    function perform_approve(nonce_: uint256, signer: address) -> () {
        require(nonce_ < operations_count, Error(0x12345678)); // OperationNotFound()

        // TODO: emit log

        match status[nonce_] {
            | OperationStatus.Approvals(count) =>
                require(votes[nonce_][signer] == Vote.None, Error(0x12345678)); // SignerAlreadyApproved()
                votes[nonce_][signer] = Vote.Approved;
                status[nonce_] = OperationStatus.Approvals(count + uint256(1));
            | _ => revertWithError(Error(0x12345678)); // UnexpectedStatus()
        }
    }

    function checkSignature(hash: bytes32, signature: Signature) -> address {
        match signature {
            | Signature.ECDSA(r, s) =>
                let signer = eip2098_signer(hash, r, s);
                require(isSigner(signer), Error(0x12345678)); // NotASigner()
                return signer;
            | Signature.Contract(contract_) =>
                require(isSigner(contract_), Error(0x12345678)); // NotASigner()
                require(check_contract_hash(contract_, hash), Error(0x12345678)); // HashNotApprovedByTarget()
                return contract_;
            | Signature.EIP1271(contract_, signature) =>
                require(isSigner(contract_), Error(0x12345678)); // NotASigner()
                require(eip1271_verify(contract_, hash, signature), Error(0x12345678)); // EIP1271VerificationRejected()
                return contract_;
        }
    }

    function create_signature_hash(kind: OperationKind, operation: Operation) -> bytes32 {
        // TODO: include domain/chaind information in hash
//        return keccak256_(concat(abi_encode(kind), abi_encode(operation)));
        // TODO: abi.encode not working yet
        return keccak256_(to_bytes(bytes32(1)));
    }

    // Anyone can call this.
    public function queueWithSignature(operation: Operation, signature: Signature) -> () {
        let hash = create_signature_hash(OperationKind.Queue, operation);

        checkSignature(hash, signature);

        perform_queue(operation);
    }

    // Anyone can call this.
    public function approveWithSignature(nonce_: uint256, signature: Signature) -> () {
        let hash = create_signature_hash(OperationKind.Approve, operations[nonce_]);

        let signer = checkSignature(hash, signature);

        perform_approve(nonce_, signer);
    }

    // Anyone can call this.
    public function rejectWithSignature(nonce_: uint256, signature: Signature) -> () {
        let hash = create_signature_hash(OperationKind.Reject, operations[nonce_]);

        let signer = checkSignature(hash, signature);

        perform_reject(nonce_, signer);
    }

/*
    public function batch(operations: array(BatchOperation)) -> () {
        for (let i = 0; i < operations.length; i += 1) {
            match operations[i] {
                | Operation.Queue(operation, signature) => queueWithSignature(operation, signature);
                | Operation.Approve(nonce_, signature) => approveWithSignature(nonce_, signature);
                | Operation.Reject(nonce_, signature) => rejectWithSignature(nonce_, signature);
                | Operation.Execute(nonce_, payload) => execute(nonce_, payload);
            }
        }
    }
*/
    // Only signers can call this.
    public function reject(nonce_: uint256) -> () {
        require(isSigner(caller()), Error(0x12345678)); // NotASigner()
        perform_reject(nonce_, caller());
    }

    function perform_reject(nonce_: uint256, signer: address) -> () {
        require(nonce_ < operations_count, Error(0x12345678)); // OperationNotFound()

        // TODO: emit log

        match status[nonce_] {
            | OperationStatus.Approvals(count) =>
                status[nonce_] = OperationStatus.Rejected;
                votes[nonce_][signer] = Vote.Rejected;
            | _ => revertWithError(Error(0x12345678)); // UnexpectedStatus()
        }
    }

    // Anyone can execute, as long as the status is correct.
    // Payload is optional, used in case UnstoredCall is encountered.
    public function execute(nonce_: uint256, payload: memory(bytes)) -> () {
        // Ensure status.
        require(nonce_ < operations_count, Error(0x12345678)); // OperationNotFound()

        // Enforce strict sequence ordering.
        require(nonce_ == nonce, Error(0x12345678)); // IncorrectSequence()
        match status[nonce_] {
            | OperationStatus.Rejected =>
                nonce += uint256(1);
                // Special case for rejections: we operate as a no-op.
                return ();
            | OperationStatus.Approvals(count) =>
                require(count >= signers_required, Error(0x12345678)); // NotEnoughApprovals()
                nonce += uint256(1);
                // Update status.
                status[nonce_] = OperationStatus.Executed;
            | _ => revertWithError(Error(0x12345678)); // IncorrectStatus();
        }

        // TODO: emit log

        // Execute.
        match operations[nonce_] {
            | Operation.AddSigner(signer) => add_signer(signer);
            | Operation.RemoveSigner(signer) => remove_signer(signer);
            | Operation.ChangeSigRequired(count) =>
                require(count <= signers_count, Error(0x12345678)); // ThresholdExceedsSigners()
                signers_required = count;
            | Operation.TransferEth(target, amount) =>
                let ret: word;
                let target_ = Typedef.rep(target);
                let amount_ = Typedef.rep(amount);
                assembly {
                    ret := call(gas(), target_, amount_, 0, 0, 0, 0)
                }
                require(tobool(ret), Error(0x12345678)); // EtherTransferFailed()
            | Operation.TransferToken(target, token, amount) =>
                safe_erc20_transfer(token, target, amount);
            | Operation.Call(target, value, payload) =>
                require(arbitrary_call(target, value, payload), Error(0x12345678)); // CallFailed()
            | Operation.UnstoredCall(hash) =>
                require(hash == keccak256_(payload), Error(0x12345678)); // InvalidPayloadSupplied()
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
            | Operation.ApproveSignedHash(hash) =>
                // Sanity check.
                require(!approved_signed_hashes[hash], Error(0x12345678)); // ApprovedSignedHashExist()
                approved_signed_hashes[hash] = true;
            | Operation.RevokeSignedHash(hash) =>
                // Sanity check.
                require(approved_signed_hashes[hash], Error(0x12345678)); // ApprovedSignedHashDoesNotExist()
                approved_signed_hashes[hash] = false;
            | _ => unimplemented(); // TODO
        }
    }

    payable fallback() -> () {
        // Accept incoming payments if no selector is hit.
        require(calldatasize() == 0, Error(0x12345678)); // UnexpectedEtherTransfer()

        // TODO: emit log
    }

    // ERC-1271 receiver
    // NOTE: view function.
    function isValidSignature(hash: bytes32, signature: memory(bytes)) -> bytes4 {
        require(approved_signed_hashes[hash], Error(0x12345678)); // HashNotApproved();
        let signature_ = Typedef.rep(signature);
        require(mload(signature_) == 0, Error(0x12345678)); // EmptySignatureExpected()
        // TODO: consider pass-through signature checking if the hash is not found (needs passing data and not hash)
        return bytes4(0x1626ba7e);
    }

    // TODO: these functions should be non-public

    // TODO: this is suboptimal
    function isSigner(signer: address) -> bool {
        for (let i = uint256(0); i < signers_count; i += uint256(1)) {
            if (signers[i] == signer) {
                return true;
            }
        }
        return false;
    }

    function add_signer(signer: address) -> () {
        require(!isSigner(signer), Error(0x12345678)); // SignerAlreadyExists()
        signers[signers_count] = signer;
        signers_count += uint256(1);
    }

    function remove_signer(signer: address) -> () {
        require(signers_count > uint256(1), Error(0x12345678)); // CannotRemoveOnlySigner()
        for (let i = uint256(0); i < signers_count; i += uint256(1)) {
            if (signers[i] == signer) {
                // Move last signer into this place.
                signers[i] = signers[signers_count - uint256(1)];
                signers_count -= uint256(1);
                // Reduce requirement if needed.
                if (signers_count < signers_required) {
                    signers_required = signers_count;
                }
                return ();
            }
        }
        revertWithError(Error(0x12345678)); // NotASigner()
    }
}

function check_contract_hash(contract__: address, hash: bytes32) -> bool {
    let ptr = get_free_memory();
    let contract_ = Typedef.rep(contract__);
    let hash_ = Typedef.rep(hash);
    let res: word;
    let ret: word;
    // We assume the [0, 32] scratch space is reserved.
    // TODO: add specific error code
    assembly {
        mstore(ptr, shl(224, 0x12345678)) // IsHashApproved(bytes32)
        mstore(add(ptr, 4), hash_)
        // Alternative option is ignoring ret, but setting mem[0] to 0.
        ret := staticcall(gas(), contract_, ptr, 36, 0, 32)
        res := mload(0)
    }
    return ret == 1 && res == 0x12345678; // Must match the magic.
}

function eip1271_verify(contract__: address, hash: bytes32, signature: memory(bytes)) -> bool {
    let ptr = get_free_memory();
    let contract_ = Typedef.rep(contract__);
    let hash_ = Typedef.rep(hash);
    let signature_ = Typedef.rep(signature);
    let res: word;
    let ret: word;
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
        ret := staticcall(gas(), contract_, ptr, add(100, size), 0, 32)
        res := mload(0)
    }
    return ret == 1 && res == 0x1626ba7e; // Must match the magic.
}

function eip2098_signer(hash: bytes32, r: bytes32, s_: bytes32) -> address {
    let s__ = Typedef.rep(s_);
    let s: word;
    let v: word;
    assembly {
        s := and(s__, sub(shl(255, 1), 1))
        v := add(shr(255, s__), 27)
    }
//    let parity = match v {
//        | 27 => Even,
//        | 28 => Odd,
//    }
    return ecrecover(hash, uint256(v), r, bytes32(s));
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

function arbitrary_call(target: address, value: uint256, payload: memory(bytes)) -> bool {
    let target_ = Typedef.rep(target);
    let value_ = Typedef.rep(value);
    let payload_ = Typedef.rep(payload);
    let ret: word;
    assembly {
        ret := call(gas(), target_, value_, add(payload_, 32), mload(payload_), 0, 0)
    }
    return tobool(ret);
}
