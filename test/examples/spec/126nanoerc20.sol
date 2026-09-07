import {*} from std;
import {address, uint256, mapping, Num, Add, Sub, Bounded, Eq, Ord, Typedef, ge, not} from std;
pragma solcore noPattersonCondition ;
pragma solcore noCoverageCondition ;
pragma solcore noBoundVariableCondition ;

function caller() returns (address) {
  let res: word;
  assembly {
     res := caller()
  }
  return address(res);
}

function myrevert( msg: (word, word) ) returns (()) {
  match (msg ) {
    case (str, len) {
       let str1 = str; let len1 = len;
       assembly { mstore(0, str1) revert(0, len1) }
  } }
}

function myrequire(cond: bool, msg: (word, word) ) returns (()) {
      if( not(cond) ) { myrevert(msg); }
}

function require1(cond: bool) returns (()) {
    myrequire (cond, (0x72657175697265313a204641494c, 14) /* "require1: FAIL" */ );
}


function nop() returns (()) { return;}

contract Uint {
  reserved : word;
  msg_sender : address; // mock msg.sender
  owner : address;
  decimals : uint256;
  totalSupply : uint256;
  balances : mapping(address => uint256);

  function mint(amount:uint256) public returns (()) {
    balances[owner] = Num.add(balances[owner], amount);
    totalSupply = Num.add(totalSupply, amount);
  }

  //     function transferFrom(address src, address dst, uint256 amt) public returns (bool) 
  function transferFrom(src:address, dst:address, amt:uint256) public returns (bool) {
     require1(ge(balances[src], amt));

     /*
     balances[src] = Num.sub(balances[src], amt);
     balances[dst] = Num.add(balances[dst], amt):uint256;
     */
     withdraw(src, amt); //workaround typechecker quirk
     deposit(dst, amt);
     return true;
  }


  function withdraw(src:address, amt:uint256) public returns (()) {
    balances[src] = Num.sub(balances[src], amt) as uint256;
  }
  
  function deposit(dst:address, amt:uint256) public returns (()) {
    balances[dst] = Num.add(balances[dst], amt) as uint256;
  }

  function init() public returns (()) {
    owner = address(0x123456789abcdef);
    msg_sender = caller();
    decimals = uint256(18);
  }
  
  function main() public returns (uint256) {
    init();
    mint(uint256(1000));
    let src : address = owner;
    transferFrom(owner, msg_sender, uint256(42));

    return balances[msg_sender] as uint256;
  }
}
