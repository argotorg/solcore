import {*} from std;
import {callvalue, calldatasize, calldataload, shr} from std.opcodes;
import {*} from std.Generic;

export {
  ABIString,
  Contract(*),
  ExecMethod,
  Fallback(*),
  Method(*),
  MethodLevelCallvalueCheck,
  NonPayable,
  Payable,
  RunContract,
  RunDispatch,
  Selector,
  SigString,
  do_exec,
  fallback_default_implementation,
  selector_matches,
  sigStr
};

pragma solcore noPattersonCondition ;
pragma solcore noCoverageCondition ;
pragma solcore noBoundVariableCondition ;

// --- Core Data Types ---

// A contract contains a tuple of methods and a single fallback
// TODO: implement receive()
enum Contract<methods, fb> { Contract(methods, fb) }

// A method contains an implementation (fn) as well as it's name and type signature
enum Method<name, payability, args, rets, fn> { Method(Proxy<name>, Proxy<payability>, Proxy<args>, Proxy<rets>, fn) }

// Contains the implementation for the fallback (fn) as well as it's type signature
enum Fallback<payability, args, rets, fn> { Fallback(Proxy<payability>, Proxy<args>, Proxy<rets>, fn) }

// --- Method Selectors ---

trait ABIString<ty> { // deprecated
    function append(head : word, tail : word, prx : Proxy<ty>) returns (word);
}

trait SigString<t> { function sigStr(x:Proxy<t>) returns (string); }

function sigStr<t>(p:Proxy<t>) returns (string)  where t: SigString { return SigString.sigStr(p); }

impl SigString<uint256> { function sigStr(x:Proxy<uint256>) returns (string) { return "uint256"; }}
impl SigString<bytes32> { function sigStr(x:Proxy<bytes32>) returns (string) { return "bytes32"; }}
impl SigString<address> { function sigStr(x:Proxy<address>) returns (string) { return "address"; }}
impl SigString<string memory> { function sigStr(x:Proxy<string memory>) returns (string) { return "string"; }}
impl SigString<bytes memory> { function sigStr(x:Proxy<bytes memory>) returns (string) { return "bytes"; }}
impl SigString<()> { function sigStr(x:Proxy<()>) returns (string) { return ""; } }

impl<a, b> SigString<(a, b)> where a: SigString, b: SigString {
  function sigStr(x:Proxy<(a, b)>) returns (string) {
    return SigString.sigStr( Proxy as Proxy<a> ) +  "," + SigString.sigStr( Proxy as Proxy<b> );
  }
}

// A tagged union (the SOP form of an ADT with several constructors). There is no
// standard ABI type for sums, so this signature is structural: the branch
// signatures wrapped in a `sum(l,r)` constructor. The explicit `sum(...)` wrapper
// keeps sums from colliding with the comma-joined product `(a,b)` signature, so
// `sum(uint256,uint256)` and `(uint256,uint256)` hash to distinct selectors. It
// makes ADT-typed parameters produce a deterministic selector; refine here if a
// specific on-the-wire sum convention is needed.
impl<f, g> SigString<sum<f, g>> where f: SigString, g: SigString {
  function sigStr(x:Proxy<sum<f, g>>) returns (string) {
    return "sum(" + SigString.sigStr( Proxy as Proxy<f> ) + "," + SigString.sigStr( Proxy as Proxy<g> ) + ")";
  }
}

// Any data type inherits its ABI signature from its Generic representation, the
// same way ABIAttribs / ABIEncode bridge through Generic in std.ABIGeneric. This
// lets the dispatch take ADT-typed parameters (e.g. a Signature) without a
// hand-written SigString instance per type.
default impl<a, rep> SigString<a> where a: Generic<rep>, rep: SigString {
  function sigStr(x:Proxy<a>) returns (string) {
    return SigString.sigStr( Proxy as Proxy<rep> );
  }
}

impl<name, f, args, rets, payability> SigString<Method<name, payability, args, rets, f>> where f: invokable<args, rets>, name: SigString, args: SigString, rets: SigString {
  function sigStr(x:Proxy<Method<name, payability, args, rets, f>>) returns (string) {
    return sigStr(Proxy as Proxy<name>)  + "(" + sigStr(Proxy as Proxy<args>) + ")";
  }
}


trait Selector<ty> {
    function compute(prx : Proxy<ty>) returns (bytes4);
}

// Computes the selector hash for a given method
// this is a class with a single instance since it made some of the downstream definitions a bit cleaner to define
// NOTE: for efficiency purposes this leaves dirty data past the end of the free memory pointer
impl<name, payability, args, rets, fn> Selector<Method<name, payability, args, rets, fn>> where name: SigString, args: SigString {
    function compute(prx : Proxy<Method<name, payability, args, rets, fn>>) returns (bytes4) {
        // let hash : word = keccakLit(sigStr(prx));
        let hash = keccakLit(sigStr(Proxy as Proxy<name>)  + "(" + sigStr(Proxy as Proxy<args>) + ")");
        return bytes4(shr(224, hash));
    }
}

// --- Method Execution ---

// Describes how to execute a given method / fallback
trait ExecMethod<ty> {
  function exec(x: ty) returns (());
}

// If fn matches the provided args/ret types, then we can execute any non-payable method
impl<name, args, rets, fn> ExecMethod<Method<name, NonPayable, args, rets, fn>> where fn: invokable<args, rets>, args: ABIAttribs, rets: ABIAttribs, ABIDecoder<args, CalldataWordReader>: ABIDecode<args>, rets: ABIEncode {
  function exec(m : Method<name, NonPayable, args, rets, fn>) returns (()) {
    match (m ) {
      case Method(pnm,ppayability,pargs,prets,fn) {
        // non-payable methods must reject any callvalue before running
        MethodLevelCallvalueCheck.checkCallvalue(Proxy as Proxy<NonPayable>);
        do_exec(pargs, prets, fn);
    } }
  }
}

// If fn matches the provided args/ret types, then we can execute any payable method
// payable methods skip the callvalue check entirely
impl<name, args, rets, fn> ExecMethod<Method<name, Payable, args, rets, fn>> where fn: invokable<args, rets>, args: ABIAttribs, rets: ABIAttribs, ABIDecoder<args, CalldataWordReader>: ABIDecode<args>, rets: ABIEncode {
  function exec(m : Method<name, Payable, args, rets, fn>) returns (()) {
    match (m ) {
      case Method(pnm,ppayability,pargs,prets,fn) {
        do_exec(pargs, prets, fn);
    } }
  }
}

// Fallbacks have no ABI-decoded inputs or outputs, so the instance is
// specialised to args = rets = () and bypasses the calldata length check
// and ABI decode/encode entirely.
impl<payability, fn> ExecMethod<Fallback<payability, (), (), fn>> where fn: invokable<(), ()>, payability: MethodLevelCallvalueCheck {
  function exec(fb : Fallback<payability, (), (), fn>) returns (()) {
    match (fb ) {
      case Fallback(ppayability, pargs, prets, fn) {
        MethodLevelCallvalueCheck.checkCallvalue(Proxy as Proxy<payability>);
        fn(());
        assembly {
          stop()
        }
    } }
  }
}

function do_exec<args, rets, fn>(pargs : Proxy<args>, prets : Proxy<rets>, fn : fn) returns (())  where fn: invokable<args, rets>, args: ABIAttribs, rets: ABIAttribs, ABIDecoder<args, CalldataWordReader>: ABIDecode<args>, rets: ABIEncode {
    // check we have enough calldata for the head of args
    require(calldatasize() >= (ABIAttribs.headSize(pargs) + 4), Error(0x08638556)); // ABIInputTruncated()

    // TODO: calldatasize checks for dynamic types

    // abi decode args from calldata
    let ptr : bytes calldata = calldata(4);

    // TODO: this needs entirely too many type annotations
    let args : args = abi_decode(ptr, pargs, Proxy as Proxy<CalldataWordReader>);

    // call fn with args
    // TODO: why are type annotations needed here?
    let rets : rets = fn(args);

    // abi encode rets to memory
    let ptr = abi_encode(rets);

    // let retSz : word = ABIAttribs.headSize(prets);
    // the approach above does not work for dynamically sized types...
    // ...instead we take the size of memory allocated by the encoding
    let start : word = Typedef.rep(ptr);
    let end : word = get_free_memory();
    let retSz : word = end - start;
    assembly {
        return(start, retSz)
    }
}

// --- Method Dispatch ---

// For a given tuple of methods this executes the method specified by the first four bytes of calldata
trait RunDispatch<ty> {
  function go(methods : ty) returns (());
}

// We can dispatch to a single executable method with a known selector
impl<name, payability, args, rets, fn> RunDispatch<Method<name, payability, args, rets, fn>> where Method<name, payability, args, rets, fn>: ExecMethod, Method<name, payability, args, rets, fn>: Selector {
  function go(method : Method<name, payability, args, rets, fn>) returns (()) {
    match (selector_matches(Proxy as Proxy<Method<name, payability, args, rets, fn>>) ) {
      case true { ExecMethod.exec(method);
      } case false { return;
    } }
  }
}

// Base case: a contract with no methods has nothing to dispatch to
impl RunDispatch<()> {
  function go(methods : ()) returns (()) { }
}

// Recursive instance
impl<n, m> RunDispatch<(n, m)> where n: ExecMethod, n: Selector, m: RunDispatch {
  function go(methods : (n, m)) returns (()) {
    match (methods ) {
      case (method_n, rest) {
        match (selector_matches(Proxy as Proxy<n>) ) {
          case true { ExecMethod.exec(method_n);
          } case false { RunDispatch.go(rest);
        } }
    } }
  }
}

// TODO: we only wanna do the calldataload once
// Given evidence of a type with a known selector, we can check if it matches the selector in the first four bytes of calldata
function selector_matches<ty>(prx : Proxy<ty>) returns (bool)  where ty: Selector {
  let candidate = Typedef.rep(Selector.compute(prx));
  let selector = shr(224, calldataload(0));
  return selector == candidate;
}

// --- Callvalue Checks ---

enum Payable {}
enum NonPayable {}

trait MethodLevelCallvalueCheck<ty> {
    function checkCallvalue(pty : Proxy<ty>) returns (());
}

// no callvalue check for Payable methods
impl MethodLevelCallvalueCheck<Payable> {
    function checkCallvalue(prx : Proxy<Payable>) returns (()) { }
}
// NonPayable methods revert if passed value
impl MethodLevelCallvalueCheck<NonPayable> {
    function checkCallvalue(prx : Proxy<NonPayable>) returns (()) {
        let NonPayableReceivedValue = Error(0xb5988ea3);
        require(callvalue() == 0, NonPayableReceivedValue);
   }
}

// --- Contract Execution ---

// Describes how to execute a given contract
trait RunContract<c> {
  function exec(v : c) returns (());
}

// If we have a dispatch for the contracts methods, and we know how to execute it's fallback, then we can define an entrypoint
impl<methods, fb> RunContract<Contract<methods, fb>> where methods: RunDispatch, fb: ExecMethod {
  function exec(c : Contract<methods, fb>) returns (()) {
    match (c ) {
      case Contract(ms, fb) {

        // TODO: if all methods are non payable then we should life the callvalue check here

        // set free memory pointer to the output of memoryguard
        // https://docs.soliditylang.org/en/v0.8.30/yul.html#memoryguard
        // TODO: we will need to consider immutables here at some point...
        assembly { mstore(0x40, memoryguard(128)) }

        // calldata shorter than 4 bytes can't contain a selector — skip
        // dispatch and invoke the fallback directly (matches Solidity)
        if (calldatasize() >= 4) {
            // dispatch to method based on selector
            RunDispatch.go(ms);
        }
        // fallthrough to fallback -- this will be reached upon short input
        //                            or no matching selector
        ExecMethod.exec(fb);
    } }
  }
}

// This is the default fallback used if none is defined.
function fallback_default_implementation() returns (()) {
  let NoSelectorMatchedWithoutFallback = Error(0x4924aef0);
  revertWithError(NoSelectorMatchedWithoutFallback);
}
