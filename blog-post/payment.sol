enum address { address(word) }

enum tokenid { tokenid(word) }

enum Payment { Native(address, word), ERC20(address, address, address, word), ERC721(address, address, address, tokenid) }

function processPayment(payment : Payment) {
    match (payment ) {
    case Native(to, amount) {
      transfer(to, amount);
    } case ERC20(token, from, to, amount) {
      transferFromERC20(from, to, amount);
    } case ERC721(token, from, to, tokenId) {
      transferFromERC721(from, to, tokenId);
    } }
}

function transfer (to : address, amount : word) {

}

function transferFromERC20 (from : address, to, amount) {

}

function transferFromERC721(from, to, tokenId) {

}
