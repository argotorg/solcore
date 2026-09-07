// test complex match example from the blog post
// simplified to use word instead of uint256

import {address, Num, Add, Sub, Div, Bounded, Eq, Ord, Typedef} from std;

enum AuctionState { NotStarted(word), Active(word, address), Ended(word, address), Cancelled(word, address) }

enum Phase { Early, Late }

function discount(state : AuctionState, phase : Phase) returns (word) {
    match (state, phase ) {
    case (.Active(bid, _), .Early ) { return bid / 10;
    } case (.Active(bid, _), .Late  ) { return bid / 20;
    } default { return 0;
    } }
}

contract Discount {
  function main() public returns (word) {
    return discount(.Active(420,.address(0)), .Early);
  }
}
