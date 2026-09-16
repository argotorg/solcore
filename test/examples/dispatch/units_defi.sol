// Dimensional analysis for DeFi with std.units, modeling the issues from the
// Trail of Bits post "Spotting issues in DeFi with dimensional analysis":
//   (3) ERC-4626 convertToAssets called with a decimals constant instead of a
//       share amount  -> here a TYPE ERROR at the function boundary;
//   (+) adding quantities of different dimensions (shares vs assets)
//       -> a TYPE ERROR (Add is same-unit only);
//   (1,2) multiply-vs-divide direction (K = A*B) -> mulq/divq yield DISTINCT
//       composite types, so the wrong operation fails to match the expected type.
//
// Each capability is a phantom "dimension" tag; a Qty<u> is a uint256 tagged with
// its dimension. std.units gives same-unit +/-, scalar *, and cross-unit
// mulq/divq producing Prod/Per composite dimensions.
import * from std;
import * from std.dispatch;
import * from std.units;

enum Asset { Asset }   // underlying asset amount   [assets]
enum Share { Share }   // vault share amount        [shares]
enum TokA  { TokA }    // AMM reserve of token A    [A]
enum TokB  { TokB }    // AMM reserve of token B    [B]

// smart constructors (a raw uint256 becomes a dimensioned quantity only here)
function assets(x : uint256) returns (Qty<Asset>) { 
  return Qty(x); 
}

function shares(x : uint256) returns (Qty<Share>) { 
  return Qty(x); 
}

function tokA(x : uint256) returns (Qty<TokA>) { 
  return Qty(x); 
}

function tokB(x : uint256) returns (Qty<TokB>) { 
  return Qty(x); 
}

// --- ERC-4626: assets = shares * assetsPerShare
// The SIGNATURE is the dimensional contract: the first argument must be a share
// amount. Passing a bare decimals constant (as the ToB post's bug does) will not
// type-check. rateWad is an 18-decimals fixed-point assets-per-share rate.
function convertToAssets(s : Qty<Share>, rateWad : uint256) returns (Qty<Asset>) {
  return assets(amount(s) * rateWad / ether(1));   // ether(1) = 1e18
}

// --- AMM constant-product invariant  K = A * B  (dimension [A]*[B])
function poolInvariant(a : Qty<TokA>, b : Qty<TokB>) returns (Qty<Prod<TokA, TokB>>) {
  return mulq(a, b);
}

contract UnitsDefi {
  // convertToAssets(3 shares, rate = 2e18) = 6 assets
  function pricePerShare() public returns (uint256) {
    return amount(convertToAssets(shares(3), ether(2)));
  }

  // poolInvariant(1000 A, 2000 B) = 2_000_000  (as a Prod<TokA,TokB> amount)
  function invariant() public returns (uint256) {
    return amount(poolInvariant(tokA(1000), tokB(2000)));
  }

  // total shares + minted shares: same dimension, so the sum is well-typed.
  function totalShares() public returns (uint256) {
    return amount(shares(100) + shares(25));
  }
}
