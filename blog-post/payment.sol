data address;

data Payment =
    Native (address,word)
  | ERC20 (address,address,address,word)  
  | ERC721(address, address,address,word);

function processPayment(payment : Payment) {
    match payment {
    | Native(to,amount) => 
      transfer(to,amount);
    | ERC20(token,from,to,amount) =>
      transferFromERC20(from, to, amount);
    | ERC721(token,from,to,tokenId) =>
      transferFromERC721(from, to, tokenId);
    }
}

function transfer (to : address, amount : word) {

}

function transferFromERC20 (from : address, to, amount) {

}

function transferFromERC721(from, to, tokenId) {

}
