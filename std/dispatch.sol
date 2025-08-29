pragma no-patterson-condition RunDispatch, ExecMethod;

// --- Preliminaries ---

data Bool = True | False;
data Proxy(a) = Proxy;

// --- Core Data Types ---

// A contract contains a tuple of methods and a single fallback
// TODO: implement receive()
data Contract(methods, fallback) = Contract(methods,fallback);

// A method contains an implementation (fn) as well as it's name and type signature
data Method(name, payability, args, rets, fn) = Method(name, payability, args, rets, fn);

// Contains the implementation for the fallback (fn) as well as it's type signature
data Fallback(payability, args, rets, fn) = Fallback(payability, args, rets, fn);

// --- Method Selectors ---

// For each method in a contract the compiler generates a unique type and
// produces a `Selector` instance for that type that returns the selector hash
forall nm . class nm:Selector {
  function hash(prx: Proxy(nm)) -> word;
}

// Method has a Selector if its name has a Selector
forall name payability args rets fn . name:Selector => instance Method(name,payability,args,rets,fn):Selector {
  function hash(prx: Proxy(Method(name,payability,args,rets,fn))) -> word {
    return Selector.hash(Proxy : Proxy(name));
  }
}

// --- Method Execution ---

// Describes how to execute a given method / fallback
forall ty . class ty:ExecMethod {
  function exec(x: ty);
}

// If fn matches the provided args/ret types, then we can execute any method
forall name payability args rets fn
  // TODO: should get a type error here...
  . fn:invokable(args,ret), Method(name,payability,Proxy(args),rets,fn):MethodLevelCallvalueCheck
  => instance Method(name,payability,Proxy(args),rets,fn):ExecMethod {
  function exec(m : Method(name,payability,Proxy(args),rets,fn)) -> () {
    match m {
      | Method(nm,payability,pargs,rets,fn) =>
        // check callvalue
        MethodLevelCallvalueCheck.checkCallvalue(Proxy : Proxy(Method(name,payability,Proxy(args),rets,fn)));

        // check we have enough calldata for the head of args
        // abi decode args from calldata
        // call fn with args
        // abi encode rets to memory
        // returndata copy encoded returns
        // evm return
        return ();
    }
  }
}

// If fn matches the provided args/ret types, then we can execute any fallback
forall payability args rets fn
  . fn:invokable(args,rets), Fallback(payability,Proxy(args),Proxy(rets),fn):MethodLevelCallvalueCheck
  => instance Fallback(payability,Proxy(args),Proxy(rets),fn):ExecMethod {
  function exec(fb : Fallback(payability, Proxy(args),Proxy(rets),fn)) -> () {
    match fb {
      | Fallback(payability, args, rets, fn) =>
        // check callvalue
        MethodLevelCallvalueCheck.checkCallvalue(Proxy : Proxy(Fallback(payability, Proxy(args),Proxy(rets),fn)));

        // check we have enough calldata for the head of args
        // abi decode args from calldata
        // call fn with args
        // abi encode rets to memory
        // returndata copy encoded returns
        // evm return
        return ();
    }
  }
}

// --- Method Dispatch ---

// For a given tuple of methods this executes the method specified by the first four bytes of calldata
forall ty callvalueCheckStatus . class ty:RunDispatch {
  function go(methods : ty) -> ();
}

// We can dispatch to a single executable method with a known selector
// TODO: do we need this instance?
forall m . m:ExecMethod, m:Selector => instance m:RunDispatch {
  function go(method : m) -> () {
    match selector_matches(Proxy : Proxy(m)) {
      | True => ExecMethod.exec(method);
      | False => return ();
    }
  }
}

// We can dispatch to a tuple of executable methods with a known selector
forall n m . n:ExecMethod, n:Selector, m:ExecMethod, m:Selector => instance (n,m):RunDispatch {
  function go(methods : (n,m)) -> () {
    match methods {
      | (method_n, method_m) =>
        match selector_matches(Proxy : Proxy(n)) {
          | True => ExecMethod.exec(method_n);
          | False => match selector_matches(Proxy : Proxy(m)) {
            | True => ExecMethod.exec(method_m);
            | False => return ();
          }
        }
    }
  }
}

// Recursive instance
forall n m . n:ExecMethod, n:Selector, m:RunDispatch => instance (n,m):RunDispatch {
  function go(methods : (n,m)) -> () {
    match methods {
      | (method_n, rest) =>
        match selector_matches(Proxy : Proxy(n)) {
          | True => ExecMethod.exec(method_n);
          | False => RunDispatch.go(rest);
        }
    }
  }
}

// TODO: we only wanna do the calldataload once
// Given evidence of a name with a known selector, we can check if it matches the selector in the first four bytes of calldata
forall name . name:Selector => function selector_matches(prx : Proxy(name)) -> Bool {
  let hash = Selector.hash(prx);
  let res : word;
  assembly {
    let sel := shr(224, calldataload(0));
    res := eq(sel, hash);
  }
  match res {
    | 0 => return False;
    | _ => return True;
  }
}

// --- Callvalue Checks ---

data Payable;
data NonPayable;

forall ty . class ty:MethodLevelCallvalueCheck {
  function checkCallvalue(pty : Proxy(ty));
}

// no callvalue check for Payable methods
forall name args ret fn . instance Method(name,Proxy(Payable),args,ret,fn):MethodLevelCallvalueCheck {
  function checkCallvalue(prx : Proxy(Method(name,Proxy(Payable),args,ret,fn))) -> () { }
}
forall args ret fn . instance Fallback(Proxy(Payable),args,ret,fn):MethodLevelCallvalueCheck {
  function checkCallvalue(prx : Proxy(Fallback(Proxy(Payable),args,ret,fn))) -> () { }
}

// NonPayable methods revert if passed value
forall name args ret fn . instance Method(name,Proxy(NonPayable),args,ret,fn):MethodLevelCallvalueCheck {
  function checkCallvalue(prx : Proxy(Method(name,Proxy(NonPayable),args,ret,fn))) -> () {
    ensureNoCallvalue();
  }
}

forall name args ret fn . instance Fallback(Proxy(NonPayable),args,ret,fn):MethodLevelCallvalueCheck {
  function checkCallvalue(prx : Proxy(Fallback(Proxy(NonPayable),args,ret,fn))) -> () {
    ensureNoCallvalue();
  }
}

function ensureNoCallvalue() -> () {
  assembly {
    if gt(callvalue(), 0) {
      mstore(0, 0x1);
      revert(0, 32);
    }
  }
}

// --- Contract Execution ---

// Describes how to execute a given contract
forall c . class c:RunContract {
  function exec(v : c) -> ();
}

// If we have a dispatch for the contracts methods, and we know how to execute it's fallback, then we can define an entrypoint
forall methods fallback . methods:RunDispatch, fallback:ExecMethod => instance Contract(methods, fallback):RunContract {
  function exec(c : Contract(methods, fallback)) -> () {
    match c {
      | Contract(ms, fb) =>

        // TODO: if all methods are non payable then we should life the callvalue check here

        // check that we have at least 4 bytes of calldata
        let haveSelector : word;
        assembly {
          haveSelector := lt(3, calldatasize());
        }

        match haveSelector {
          | 0 => assembly {
	          mstore(0,0xff)
            revert(0,1)
          }
          | _ =>
            // set free memory pointer to the output of memoryguard
            // https://docs.soliditylang.org/en/v0.8.30/yul.html#memoryguard
            // TODO: we will need to consider immutables here at some point...
            assembly { mstore(0x40, memoryguard(128)); }

            // dispatch to method based on selector
            RunDispatch.go(ms);
            // run fallback if no methods matched
            ExecMethod.exec(fb);
        }
    }
  }
}

// --- Manually Desugared Example ---

// compiler generated

function revert_handler() -> () {
  assembly { revert(0,0) }
}

data C_Add2_Selector = C_Add2_Selector;

instance C_Add2_Selector:Selector {
  function hash(prx: Proxy(C_Add2_Selector)) -> word {
    // This would be keccak256("add2(uint256,uint256)") >> 224
    // Compiler computes this at compile time
    return 0x29fcda33;  // placeholder value
  }
}

// transform

contract C {
  function add2(x : word, y : word) -> word {
    let ret : word;
    assembly { ret := add(x,y) }
    return ret;
  }

  function main() -> word {
    let c = Contract(
      Method(C_Add2_Selector, Proxy : Proxy(NonPayable), Proxy : Proxy((word,word)), Proxy : Proxy(word), add2),
      Fallback(Proxy : Proxy(NonPayable), Proxy : Proxy(()), Proxy : Proxy(()),revert_handler)
    );

    RunContract.exec(c);
    return 0;
  }
}
