pragma no-patterson-condition RunDispatch;

data bool = true | false;
data Proxy(a) = Proxy;

class nm:Selector {
  function hash(prx: Proxy(nm)) -> word;
}

class ty:ExecMethod {
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

class c:RunContract {
  function exec(v : c);
}
forall methods fallback . instance Contract(methods, fallback):RunContract {
  function exec(prx: Proxy(Contract(methods, fallback))) {
    // set free memory pointer to the output of memoryguard (TODO: what is memoryguard again)
    // check that we have at least 4 bytes
    // check callvalue (TODO: we need some typeclass magic here)
    RunDispatch.go(Proxy : Proxy(methods));
    ExecMethod.exec(Proxy : Proxy(fallback));
  }
}

data Method(name, args, rets, fn) = Method(name, args, rets, fn);

forall name args rets fn . instance Method(name,args,rets,fn):ExecMethod {
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

// TODO: we need this to lift the non payable check to the top level if everything is non payable
class ty:NonPayable {}
class ty:AllNonPayable {}

forall n m . n:NonPayable, m:AllNonPayable => instance (n,m):AllNonPayable {}



// TODO: we only wanna do the calldataload once
forall name . name:Selector => function selector_matches(nm : name) -> bool {
  let sel : word;
  assembly { let sel := shr(224, calldataload(0)) }
  match primEqWord(Selector.hash(Proxy : Proxy(name)), sel) {
    | 0 => return true;
    | 1 => return false;
  }
}

class ty:RunDispatch {
  function go(prx : Proxy(ty)) -> ();
}

forall n . n:ExecMethod => instance n:RunDispatch {
  function go(m) {
    match selector_matches(Proxy : Proxy(m)) {
      | true => ExecMethod.exec(Proxy : Proxy(m));
      | false => return ();
    }
  }
}

forall n m . n:ExecMethod, m:ExecMethod => instance (n,m):RunDispatch {
  function go(ds) {
    match selector_matches(Proxy : Proxy(n)) {
      | true => ExecMethod.exec(Proxy : Proxy(n));
      | false => match selector_matches(Proxy : Proxy(m)) {
        | true => ExecMethod.exec(Proxy : Proxy(m));
        | false => return ();
      }
    }
  }
}

forall n m . n:ExecMethod, m:RunDispatch => instance (n,m):RunDispatch {
  function go(ds) {
    match selector_matches(Proxy : Proxy(n)) {
      | true => ExecMethod.exec(Proxy : Proxy(n));
      | false => RunDispatch.go(Proxy : Proxy(m));
    }
  }
}

// compiler generated

function revert_handler() -> () {
  assembly { revert(0,0) }
}

data C_Add2_Selector = C_Add2_Selector;

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
