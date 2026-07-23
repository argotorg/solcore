
// --- Preliminaries ---

enum Bool { True, False }
enum Proxy<a> { Proxy }

// --- Core Data Types ---

// A contract contains a tuple of methods and a single fallback
// TODO: implement receive()
enum Contract<methods, fb> { Contract(methods, fb) }

// A method contains an implementation (fn) as well as it's name and type signature
enum Method<name, args, rets, fn> { Method(name, args, rets, fn) }

// Contains the implementation for the fallback (fn) as well as it's type signature
enum Fallback<args, rets, fn> { Fallback(args, rets, fn) }

// --- Method Selectors ---

// For each method in a contract the compiler generates a unique type and
// produces a `Selector` instance for that type that returns the selector hash
trait Selector<nm> {
  function hash(prx: Proxy<nm>) returns (word);
}

// Method has a Selector if its name has a Selector
impl<name, args, rets, fn> Selector<Method<name, args, rets, fn>> where name: Selector {
  function hash(prx: Proxy<Method<name, args, rets, fn>>) returns (word) {
    return Selector.hash(Proxy as Proxy<name>);
  }
}

// --- Method Execution ---

// Describes how to execute a given method / fallback
trait ExecMethod<ty> {
  function exec(x: ty, pstatus : Proxy<callvalueCheckStatus>) returns (());
}

// If fn matches the provided args/ret types, then we can execute any method
impl<name, args, rets, fn, callvalueCheckStatus> ExecMethod<Method<name, Proxy<args>, Proxy<rets>, fn>> where fn: invokable<args, ret> {
  function exec(m : Method<name, args, rets, fn>, pstatus : Proxy<callvalueCheckStatus>) returns (()) {
    match (m ) {
      case Method(nm,args,rets,fn) {
        // check callvalue
        MethodLevelCallvalueCheck.checkCallvalue(Proxy as Proxy<Method<name, args, rets, fn>>, pstatus);

        // check we have enough calldata for the head of args
        // abi decode args from calldata
        // call fn with args
        // abi encode rets to memory
        // returndata copy encoded returns
        // evm return
        return;
    } }
  }
}

// If fn matches the provided args/ret types, then we can execute any fallback
impl<args, rets, fn, callvalueCheckStatus> ExecMethod<Fallback<Proxy<args>, Proxy<rets>, fn>> where fn: invokable<args, ret> {
  function exec(fb : Fallback<args, rets, fn>, pstatus : Proxy<callvalueCheckStatus>) returns (()) {
    match (fb ) {
      case Fallback(args, rets, fn) {
        // check callvalue
        MethodLevelCallvalueCheck.checkCallvalue(Proxy as Proxy<Fallback<args, rets, fn>>, pstatus);

        // check we have enough calldata for the head of args
        // abi decode args from calldata
        // call fn with args
        // abi encode rets to memory
        // returndata copy encoded returns
        // evm return
        return;
    } }
  }
}

// --- Method Dispatch ---

// For a given tuple of methods this executes the method specified by the first four bytes of calldata
trait RunDispatch<ty> {
  function go(methods : ty, pstatus : Proxy<callvalueCheckStatus>) returns (());
}

// We can dispatch to a single executable method with a known selector
// TODO: do we need this instance?
impl<m, callvalueCheckStatus> RunDispatch<m> where m: ExecMethod, m: Selector {
  function go(method : m, pstatus : Proxy<callvalueCheckStatus>)  returns (()) {
    match (selector_matches(Proxy as Proxy<m>) ) {
      case Bool.True { ExecMethod.exec(method, pstatus);
      } case Bool.False { return;
    } }
  }
}

// We can dispatch to a tuple of executable methods with a known selector
impl<n, m, callvalueCheckStatus> RunDispatch<(n, m)> where n: ExecMethod, n: Selector, m: ExecMethod, m: Selector {
  function go(methods : (n, m), pstatus : Proxy<callvalueCheckStatus>) returns (()) {
    match (methods ) {
      case (method_n, method_m) {
        match (selector_matches(Proxy as Proxy<n>) ) {
          case Bool.True { ExecMethod.exec(method_n);
          } case Bool.False { match (selector_matches(Proxy as Proxy<m>) ) {
            case Bool.True { ExecMethod.exec(method_m, pstatus);
            } case Bool.False { return;
          } }
        } }
    } }
  }
}

// Recursive instance
impl<n, m, callvalueCheckStatus> RunDispatch<(n, m)> where n: ExecMethod, n: Selector, m: RunDispatch {
  function go(methods : (n, m), pstatus : Proxy<callvalueCheckStatus>) returns (()) {
    match (methods ) {
      case (method_n, rest) {
        match (selector_matches(Proxy as Proxy<n>) ) {
          case Bool.True { ExecMethod.exec(method_n, pstatus);
          } case Bool.False { RunDispatch.go(rest, pstatus);
        } }
    } }
  }
}

// TODO: we only wanna do the calldataload once
// Given evidence of a name with a known selector, we can check if it matches the selector in the first four bytes of calldata
function selector_matches<name>(prx : Proxy<name>) returns (Bool)  where name: Selector {
  let hash = Selector.hash(prx);
  let res : word;
  assembly {
    let sel := shr(224, calldataload(0))
    res := eq(sel, hash)
  }
  match (res ) {
    case 0 { return Bool.False;
    } default { return Bool.True;
  } }
}

// --- Callvalue Checks ---

// If every method on a contract is non payable, we lift the callvalue check to run before method dispatch
// NonPayable instances should be generated by the compiler as part of desugaring
trait NonPayable<ty> {}
trait AllNonPayable<ty> {}
impl<n, m> AllNonPayable<(n, m)> where n: NonPayable, m: AllNonPayable {}


enum CallvalueChecked {}

enum CallvalueUnchecked {}
trait MethodsMustCheckCalldata<ty> {}
impl MethodsMustCheckCalldata<CallvalueUnchecked> {}

// If every method is non payable we run the callvalue check before method dispatch
trait TopLevelCallvalueCheck<ty, ret> {
  function checkCallvalue(prx : Proxy<ty>) returns (Proxy<ret>);
}

default impl<methods> TopLevelCallvalueCheck<methods, CallvalueUnchecked> {
  function checkCallvalue(prx : Proxy<methods>) returns (Proxy<CallvalueUnchecked>) { return Proxy as Proxy<CallvalueUnchecked>; }
}

impl<methods> TopLevelCallvalueCheck<methods, CallvalueChecked> where methods: AllNonPayable {
  function checkCallvalue(prx : Proxy<methods>) returns (Proxy<CallvalueChecked>) {
    assembly {
      if gt(callvalue(), 0) {
        mstore(0,0x2)
        revert(0,32)
      }
    }
    return Proxy as Proxy<CallvalueChecked>;
  }
}

// If only some methods are non payable, then we run the check during method execution
trait MethodLevelCallvalueCheck<ty> {
  function checkCallvalue(pty : Proxy<ty>, pstatus : Proxy<status>) returns (());
}

default impl<method, status> MethodLevelCallvalueCheck<method> {
  function checkCallvalue(pty : Proxy<method>, pstatus : Proxy<status>) returns (()) { }
}

impl<method, status> MethodLevelCallvalueCheck<method> where method: NonPayable, status: MethodsMustCheckCalldata {
  function checkCallvalue(pty : Proxy<method>, pstatus : Proxy<status>) returns (()){
    assembly {
      if gt(callvalue(), 0) {
        mstore(0, 0x1)
        revert(0, 32)
      }
    }
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
        // set free memory pointer to the output of memoryguard
        // https://docs.soliditylang.org/en/v0.8.30/yul.html#memoryguard
        // TODO: we will need to consider immutables here at some point...
        // assembly { mstore(0x40, memoryguard(128)) }

        // if all methods are non payable then check callvalue
        let callvalueChecked = TopLevelCallvalueCheck.checkCallvalue(Proxy as Proxy<(fb, methods)>);

        // check that we have at least 4 bytes of calldata
        let haveSelector : word;
        assembly {
          haveSelector := lt(3, calldatasize())
        }

        match (haveSelector ) {
          case 0 { assembly { revert(0,0) }
          } default {
            // dispatch to method based on selector
            RunDispatch.go(ms, callvalueChecked);
            // run fallback if no methods matched
            ExecMethod.exec(fb);
        } }
    } }
  }
}

// --- Manually Desugared Example ---

// compiler generated

function revert_handler() returns (()) {
  assembly { revert(0,0) }
}

enum C_Add2_Selector { C_Add2_Selector }

impl Selector<C_Add2_Selector> {
  function hash(prx: Proxy<C_Add2_Selector>) returns (word) {
    // This would be keccak256("add2(uint256,uint256)") >> 224
    // Compiler computes this at compile time
    return 0x29fcda33;  // placeholder value
  }
}

// transform

contract C {
  function add2(x : word, y : word) public returns (word) {
    let ret : word;
    assembly { ret := add(x,y) }
    return ret;
  }

  function main() public returns (word) {
    let c = Contract(
      Method(C_Add2_Selector, Proxy as Proxy<(word, word)>, Proxy as Proxy<word>, add2),
      Fallback(Proxy as Proxy<()>,Proxy as Proxy<()>,revert_handler)
    );

    RunContract.exec(c);
    return 0;
  }
}
