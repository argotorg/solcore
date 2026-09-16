// Runtime checks for std.units on evmone: denomination conversion, same-unit
// arithmetic, the plain-constant layer, composite units, and truncation.
import * from std;
import * from std.dispatch;
import * from std.units;

contract Units {

  // 1 ether == 10^18 wei
  function etherToWei() public returns (uint256) {
    return weiOf(ethers(1));
  }

  // 3 gwei == 3 * 10^9 wei
  function gweiToWei() public returns (uint256) {
    return weiOf(gweis(3));
  }

  // 2 ether == 2 * 10^9 gwei
  function etherToGwei() public returns (uint256) {
    let g : Qty<Gwei> = convert(ethers(2));
    return amount(g);
  }

  // (2 + 5) ether in wei == 7 * 10^18
  function addEthers() public returns (uint256) {
    let s : Qty<Ether> = ethers(2) + ethers(5);
    return weiOf(s);
  }

  // plain layer: 1 ether + 1 gwei, in wei
  function plainEther() public returns (uint256) {
    return ether(1) + gwei(1);
  }

  // composite product: amounts 3 * 4 == 12
  function mulComposite() public returns (uint256) {
    let m : Qty<Prod<Gwei, Ether>> = mulq(gweis(3), ethers(4));
    return amount(m);
  }

  // composite ratio: amounts 10 / 2 == 5
  function divComposite() public returns (uint256) {
    let r : Qty<Per<Ether, Gwei>> = divq(ethers(10), gweis(2));
    return amount(r);
  }

  // truncation: 1.5e18 wei converted to ether floors to 1
  function weiToEtherTrunc() public returns (uint256) {
    let e : Qty<Ether> = convert(fromWei(uint256(1500000000000000000)));
    return amount(e);
  }
}
