import * from std;
import * from std.dispatch;
import * from std.units;

contract UnitsBasic {
  // 1 ether == 10^18 wei
  function etherToWei() public returns (uint256) {
    return weiOf(ethers(1));
  }

  // 3 gwei == 3 * 10^9 wei, via convert to Wei
  function gweiToWei() public returns (uint256) {
    return weiOf(gweis(3));
  }

  // 2 ether converted to gwei == 2 * 10^9
  function etherToGwei() public returns (uint256) {
    let g : Qty<Gwei> = convert(ethers(2));
    return amount(g);
  }

  // same-unit addition stays in the unit
  function addEthers() public returns (uint256) {
    let s : Qty<Ether> = ethers(2) + ethers(5);
    return weiOf(s);
  }

  // plain-constant layer (Solidity `x ether` analogue), result in wei
  function plainEther() public returns (uint256) {
    return ether(1) + gwei(1);
  }

  // composite units: (3 gwei) * (4 ether) as a raw product of amounts
  function mulComposite() public returns (uint256) {
    let m : Qty<Prod<Gwei, Ether>> = mulq(gweis(3), ethers(4));
    return amount(m);
  }

  // composite units: (10 ether) / (2 gwei) as a raw ratio of amounts
  function divComposite() public returns (uint256) {
    let r : Qty<Per<Ether, Gwei>> = divq(ethers(10), gweis(2));
    return amount(r);
  }
}
