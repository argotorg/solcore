// TUVA: TUple-based Value Access
/*
# Types and classes for assignemnt desugaring using
- access proxy types
- LValue and RValue access classes (LVA, RVA)
- StorageType class
- Assign class
*/

import * from std hiding {LValueIdxAccess, RValueIdxAccess, readStorage};
import {Typedef, storage, mapping, address, hash2, StorageType, Assign} from std;
pragma no-patterson-condition ;
pragma no-coverage-condition ;
pragma no-bounded-variable-condition ;


trait RValueIdxAccess<col_idx, val> {
  function lookup(ci : col_idx) returns (val);
}

trait LValueIdxAccess<col_idx, ref> {
  function lookup(ci : col_idx) returns (ref);
}

impl<i, a> LValueIdxAccess<(storage<mapping(i => a)>, i), storage<a>> where i: Typedef<word> {
  function lookup(xi : (storage<mapping(i => a)>, i)) returns (storage<a>) {
    match(xi) {
      case (x, i) { return storage(hash2(Typedef.rep(x), Typedef.rep(i)));
    } }

    // return storage(42); // FIXME: hash2(x,i);
  }
}

impl<i, a> RValueIdxAccess<(storage<mapping(i => a)>, i), a> where a: StorageType, i: Typedef<word> {
  function lookup(xi : (storage<mapping(i => a)>, i)) returns (a) {
  /*
    match(xi) {
      | (x, i) => return StorageType.load(hash2(Typedef.rep(x), Typedef.rep(i)));
    }
  */
  return readStorage(LValueIdxAccess.lookup(xi));
  }
}

function readStorage<a>(x:storage<a>) returns (a)  where a: StorageType {
  return StorageType.load(Typedef.rep(x));
}

function idx_rval<r, a>(x:r) returns (a)  where r: RValueIdxAccess<a> {
  return RValueIdxAccess.lookup(x);
}

function idx_lval<r, a>(x:r) returns (a)  where r: LValueIdxAccess<a> {
  return LValueIdxAccess.lookup(x);
}

contract TestTuva {
  function main() public returns (word) {
    let balances : storage<mapping(address => word)>;
    let allowances : storage<mapping(address => mapping(address => word))>;
    let ref1 : storage<word> = idx_lval( (balances, address(17)) );
    Assign.assign(idx_lval( (balances, address(1)) ), 1337);

    let ref2a // : storage( mapping(address, word) ) // omitting this type makes instance resolution fail
              = idx_lval ( (allowances, address(1)) );

    let ref2b // : storage( word )
              = idx_lval ( (ref2a, address(2)) );

    Assign.assign( ref2b, 777 );

//    return idx_rval( (balances, address(1)) );
    return idx_rval ( (ref2a, address(2)) );
  }
}
