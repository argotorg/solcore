import {*} from std;
import {address, uint256, mapping, Num, Add, Sub, Bounded, Eq, Ord, Typedef, ge, ne, not} from std;
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

function require1fail() returns (()) {
  let res: word;
  assembly {
    mstore(0x0, 0x72657175697265313a204641494c) // "require1: FAIL"
    revert(0,32)
  }
  return; // for the typechecker
}

function require1(cond: bool) returns (()) {
    match (cond ) {
    case false { return require1fail();
    } case true { return;
  } }
}

function nop() returns (()) { return;}

contract Mini {
  reserved : word;
  msg_sender : address; // mock msg.sender
  owner : address;
  decimals : uint256;
  totalSupply : uint256;
  balances : mapping(address => uint256);
  allowance : mapping(address => mapping(address => uint256));

  function mint(amount:uint256) public returns (()) {
    balances[owner] = Num.add(balances[owner], amount);
    totalSupply = Num.add(totalSupply, amount);
  }

/*  // original:
    function transferFrom(address src, address dst, uint256 amt) public returns (bool) {
        require(balanceOf[src] >= amt, "token/insufficient-balance");
        if (src != msg.sender && allowance[src][msg.sender] != type(uint256).max) {
            require(allowance[src][msg.sender] >= amt, "token/insufficient-allowance");
            allowance[src][msg.sender] -= amt;
        }

        balanceOf[src] -= amt;
        balanceOf[dst] += amt;
        emit Transfer(src, dst, amt);
        return true;
    }
*/

//  function transferFrom(src:address, dst:address, amt:uint256) -> bool {
  function transferFrom(src : address, dst : address, amt : uint256) public returns (bool)  {
     require1(ge(balances[src], amt));

     match (Eq.eq(src, msg_sender)) {
       case true { match (ne(allowance[src][msg_sender], Num.maxVal() as uint256) ) {
           case true { require1(false);
	   } case false { ();
	   } }
       } case false { ();
     } }

/*
     if ((src != msg_sender) && (allowance [src][msg_sender] != (Num.maxVal():uint256)) ) {
       require1(allowance[src][msg.sender] >= amt);
     }
*/   
     balances[src] = Num.sub(balances[src], amt);
     balances[dst] = Num.add(balances[dst], amt) as uint256;
     return true;
  }

/*
    function approve(address usr, uint256 amt) public returns (bool) {
        allowance[msg.sender][usr] = amt;
        emit Approval(msg.sender, usr, amt);
        return true;
    }
*/


  function init() public returns (()) {
    owner = address(0x123456789abcdef);
    msg_sender = caller();
    decimals = uint256(18);
  }

  function main() public returns (uint256) {
    init();
    mint(uint256(1000));
    allowance[owner][msg_sender] = uint256(10000);
    transferFrom(owner, msg_sender, uint256(42));

    return balances[msg_sender] as uint256;
  }
}
