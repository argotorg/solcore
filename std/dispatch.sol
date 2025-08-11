pragma no-patterson-condition RunDispatch;

data bool = true | false;
data Proxy(a) = Proxy;

forall nm . class nm:Selector {
  function hash(prx: Proxy(nm)) -> word;
}

forall ty . class ty:ExecMethod {
  function exec(x: ty);
}

data Fallback(args, rets, fn) = Fallback(args, rets, fn);

forall args rets fn . instance Fallback(args,rets,fn):ExecMethod {
  function exec(fb) {
    match fb {
      | Fallback(args, rets, fn) =>
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


data Contract(methods, fallback) = Contract(methods,fallback);

forall c . class c:RunContract {
  function exec(v : c);
}
forall methods fallback . methods:RunDispatch, fallback:ExecMethod => instance Contract(methods, fallback):RunContract {
  function exec(c : Contract(methods, fallback)) {
    match c {
      | Contract(ms, fb) =>
        // set free memory pointer to the output of memoryguard (TODO: what is memoryguard again)
        // check that we have at least 4 bytes
        // check callvalue (TODO: we need some typeclass magic here)
        RunDispatch.go(ms);
        ExecMethod.exec(fb);
    }
  }
}

data Method(name, args, rets, fn) = Method(name, args, rets, fn);

forall name args rets fn . fn:Invokable(args,ret) => instance Method(name,args,rets,fn):ExecMethod {
  function exec(m) {
    match m {
      | Method(nm,args,rets,fn) =>
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

// Method has a Selector if its name has a Selector
forall name args rets fn . name:Selector => instance Method(name,args,rets,fn):Selector {
  function hash(prx: Proxy(Method(name,args,rets,fn))) -> word {
    // Delegate to the name's selector
    return Selector.hash(Proxy : Proxy(name));
  }
}

// TODO: we need this to lift the non payable check to the top level if everything is non payable
forall ty . class ty:NonPayable {}
forall ty . class ty:AllNonPayable {}

forall n m . n:NonPayable, m:AllNonPayable => instance (n,m):AllNonPayable {}



// TODO: we only wanna do the calldataload once
forall name . name:Selector => function selector_matches(prx : Proxy(name)) -> bool {
  let sel : word;
  assembly { sel := shr(224, calldataload(0)) }
  match primEqWord(Selector.hash(prx), sel) {
    | 0 => return true;
    | 1 => return false;
  }
}

forall ty . class ty:RunDispatch {
  function go(methods : ty) -> ();
}

forall m . m:ExecMethod, m:Selector => instance m:RunDispatch {
  function go(method : m) {
    match selector_matches(Proxy : Proxy(m)) {
      | true => ExecMethod.exec(method);
      | false => return ();
    }
  }
}

forall n m . n:ExecMethod, n:Selector, m:ExecMethod, m:Selector => instance (n,m):RunDispatch {
  function go(methods : (n,m)) {
    match methods {
      | (method_n, method_m) =>
        match selector_matches(Proxy : Proxy(n)) {
          | true => ExecMethod.exec(method_n);
          | false => match selector_matches(Proxy : Proxy(m)) {
            | true => ExecMethod.exec(method_m);
            | false => return ();
          }
        }
    }
  }
}

forall n m . n:ExecMethod, n:Selector, m:RunDispatch => instance (n,m):RunDispatch {
  function go(methods : (n,m)) {
    match methods {
      | (method_n, rest) =>
        match selector_matches(Proxy : Proxy(n)) {
          | true => ExecMethod.exec(method_n);
          | false => RunDispatch.go(rest);
        }
    }
  }
}

// compiler generated

function revert_handler() -> () {
  assembly { revert(0,0) }
}

data C_Add2_Selector = C_Add2_Selector;

instance C_Add2_Selector:Selector {
  function hash(prx: Proxy(C_Add2_Selector)) -> word {
    // This would be keccak256("add2(uint256,uint256)") >> 224
    // Compiler computes this at compile time
    return 0x771602f7;  // placeholder value
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
    let c = Contract(Method(C_Add2_Selector, Proxy : Proxy((word,word)), Proxy : Proxy(word), add2), Fallback(Proxy : Proxy(()),Proxy : Proxy(()),revert_handler));
    RunContract.exec(c);
    return 0;
  }
}
