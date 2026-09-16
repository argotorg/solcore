// Units of measure for Core Solidity.
//
// A unit is a phantom tag type; a Qty<u> is a uint256 amount tagged with its
// unit u. Each unit carries a RATIONAL scale (num/den) relative to the base of
// its dimension (for currency, the base is Wei). Conversions multiply by the
// source scale and divide by the target scale, so quantities of the same
// dimension convert exactly (truncating only at the final integer division).
//
// Each unit's Unit<u, d> instance records its dimension d, and convert
// relates only units of the SAME dimension, so converting across dimensions
// (e.g. Ether -> Gas) is a type error, and so is adding Qty<Gwei> to Qty<Ether>
// without converting. What is NOT modeled (the compiler has no type-level
// naturals) is a normalized exponent vector: composite tags like Prod<a,b> are
// not commuted/cancelled, so A * (B/A) is not seen as B.
import * from std;

// The composite-unit instances derive a unit's dimension from its parts, so the
// unit type does not syntactically determine the dimension (it does so only
// transitively). Relax coverage/Patterson for Unit, as std does for ABIDecode.
pragma no-coverage-condition Unit;
pragma no-patterson-condition Unit;

export {
  Wei, Kwei, Mwei, Gwei, Szabo, Finney, Ether,
  Money,
  Prod(*), Per(*), Qty(*), Unit,
  pow10,
  amount, qty, scaleBy, mulq, divq,
  convert, toWei, fromWei, weiOf,
  wei, kwei, mwei, gwei, szabo, finney, ether,
  weis, kweis, mweis, gweis, szabos, finneys, ethers
};

// --- Ethereum denominations (phantom unit tags) ---
enum Wei    { Wei }
enum Kwei   { Kwei }     // 10^3  wei  (babbage)
enum Mwei   { Mwei }     // 10^6  wei  (lovelace)
enum Gwei   { Gwei }     // 10^9  wei  (shannon)
enum Szabo  { Szabo }    // 10^12 wei  (microether)
enum Finney { Finney }   // 10^15 wei  (milliether)
enum Ether  { Ether }    // 10^18 wei

// --- Dimension tags ---
// Every denomination above is the same dimension: value/money. A unit's Unit
// instance records its dimension, and convert only relates units of the SAME
// dimension. Add your own dimensions for other quantities (e.g. enum Gas { Gas }).
enum Money { Money }

// --- comptime 10^n, folded to a literal (see doc/comptime-integer.md) ---
function pow10(comptime n : integer) returns (comptime<integer>) {
  if (integerLt(n, 1)) {
    return 1;
  } else {
    return integerMul(10, pow10(integerSub(n, 1)));
  }
}

function pow10u(comptime n : integer) returns (uint256) {
  return uint256(wordFromInteger(pow10(n)));
}

// --- Unit: a per-unit rational scale, tagged with its DIMENSION d. ---
// Dispatch is on the unit u; d is a phantom dimension tag fixed by the
// instance. convert requires both units to share a dimension (below), so
// converting across dimensions (e.g. Ether -> Gas) is a type error.
trait Unit<u, d> {
  function scaleNum(p : Proxy<u>) returns (uint256);
  function scaleDen(p : Proxy<u>) returns (uint256);
}

impl Unit<Wei, Money> {
  function scaleNum(p : Proxy<Wei>) returns (uint256) { 
    return uint256(1); 
  }

  function scaleDen(p : Proxy<Wei>) returns (uint256) { 
    return uint256(1); 
  }
}
impl Unit<Kwei, Money> {
  function scaleNum(p : Proxy<Kwei>) returns (uint256) { 
    return pow10u(3); 
  }

  function scaleDen(p : Proxy<Kwei>) returns (uint256) { 
    return uint256(1); 
  }
}
impl Unit<Mwei, Money> {
  function scaleNum(p : Proxy<Mwei>) returns (uint256) { 
    return pow10u(6); 
  }
  
  function scaleDen(p : Proxy<Mwei>) returns (uint256) { 
    return uint256(1); 
  }
}
impl Unit<Gwei, Money> {
  function scaleNum(p : Proxy<Gwei>) returns (uint256) { 
    return pow10u(9); 
  }
  
  function scaleDen(p : Proxy<Gwei>) returns (uint256) { 
    return uint256(1); 
  }
}
impl Unit<Szabo, Money> {
  function scaleNum(p : Proxy<Szabo>) returns (uint256) { 
    return pow10u(12); 
  }
  
  function scaleDen(p : Proxy<Szabo>) returns (uint256) { 
    return uint256(1); 
  }
}
impl Unit<Finney, Money> {
  function scaleNum(p : Proxy<Finney>) returns (uint256) { 
    return pow10u(15); 
  }
  
  function scaleDen(p : Proxy<Finney>) returns (uint256) { 
    return uint256(1); 
  }
}
impl Unit<Ether, Money> {
  function scaleNum(p : Proxy<Ether>) returns (uint256) { 
    return pow10u(18); 
  }
  
  function scaleDen(p : Proxy<Ether>) returns (uint256) { 
    return uint256(1); 
  }
}

// --- Qty<u>: a uint256 amount tagged with its unit ---
enum Qty<u> { Qty(uint256) }

function amount<u>(q : Qty<u>) returns (uint256) {
  match (q) {
    case Qty(v) { return v; }
  }
}

function qty<u>(v : uint256, p : Proxy<u>) returns (Qty<u>) {
  return Qty(v);
}

// --- Same-unit arithmetic (mixing units is a type error) ---
impl<u> Add<Qty<u>> {
  function add(x : Qty<u>, y : Qty<u>) returns (Qty<u>) { 
    return Qty(amount(x) + amount(y)); 
  }
}
impl<u> Sub<Qty<u>> {
  function sub(x : Qty<u>, y : Qty<u>) returns (Qty<u>) { 
    return Qty(amount(x) - amount(y)); 
  }
}

function scaleBy<u>(q : Qty<u>, k : uint256) returns (Qty<u>) {
  return Qty(amount(q) * k);
}

// --- Conversion (dimension-checked, rational, exact up to the final truncation) ---
// Both units must share a dimension d; converting across dimensions does not
// type-check.  value_b = value_a * num(a)*den(b) / (den(a)*num(b))
function convert<a, b, d>(q : Qty<a>) returns (Qty<b>) where a: Unit<d>, b: Unit<d> {
  let pa : Proxy<a> = Proxy;
  let pb : Proxy<b> = Proxy;
  let n : uint256 = amount(q) * Unit.scaleNum(pa) * Unit.scaleDen(pb);
  let den : uint256 = Unit.scaleDen(pa) * Unit.scaleNum(pb);
  return Qty(n / den);
}

function toWei<a>(q : Qty<a>) returns (Qty<Wei>) where a: Unit<Money> {
  return convert(q);
}

function fromWei(w : uint256) returns (Qty<Wei>) {
  return Qty(w);
}

function weiOf<a>(q : Qty<a>) returns (uint256) where a: Unit<Money> {
  return amount(toWei(q));
}

// --- Composite units: product (Prod) and ratio (Per), scale composed generically ---
enum Prod<a, b> { Prod }
enum Per<a, b> { Per }

// A product/ratio unit's dimension is the product/ratio of the components'
// dimensions, so composite units are dimension-checked too.
impl<a, b, da, db> Unit<Prod<a, b>, Prod<da, db>> where a: Unit<da>, b: Unit<db> {
  function scaleNum(p : Proxy<Prod<a, b>>) returns (uint256) {
    let pa : Proxy<a> = Proxy;
    let pb : Proxy<b> = Proxy;
    return Unit.scaleNum(pa) * Unit.scaleNum(pb);
  }
  function scaleDen(p : Proxy<Prod<a, b>>) returns (uint256) {
    let pa : Proxy<a> = Proxy;
    let pb : Proxy<b> = Proxy;
    return Unit.scaleDen(pa) * Unit.scaleDen(pb);
  }
}

impl<a, b, da, db> Unit<Per<a, b>, Per<da, db>> where a: Unit<da>, b: Unit<db> {
  function scaleNum(p : Proxy<Per<a, b>>) returns (uint256) {
    let pa : Proxy<a> = Proxy;
    let pb : Proxy<b> = Proxy;
    return Unit.scaleNum(pa) * Unit.scaleDen(pb);
  }
  function scaleDen(p : Proxy<Per<a, b>>) returns (uint256) {
    let pa : Proxy<a> = Proxy;
    let pb : Proxy<b> = Proxy;
    return Unit.scaleDen(pa) * Unit.scaleNum(pb);
  }
}

// x : Qty<a>, y : Qty<b>  ->  Qty<Prod<a,b>> / Qty<Per<a,b>>
function mulq<a, b>(x : Qty<a>, y : Qty<b>) returns (Qty<Prod<a, b>>) {
  return Qty(amount(x) * amount(y));
}
function divq<a, b>(x : Qty<a>, y : Qty<b>) returns (Qty<Per<a, b>>) {
  return Qty(amount(x) / amount(y));
}

// --- Plain uint256 constants, already in wei (Solidity x ether analogue) ---
function wei(x : uint256) returns (uint256) { 
  return x; 
}

function kwei(x : uint256) returns (uint256) { 
  return x * pow10u(3); 
}

function mwei(x : uint256) returns (uint256) { 
  return x * pow10u(6); 
}

function gwei(x : uint256) returns (uint256) { 
  return x * pow10u(9); 
}

function szabo(x : uint256) returns (uint256) { 
  return x * pow10u(12); 
}

function finney(x : uint256) returns (uint256) { 
  return x * pow10u(15); 
}

function ether(x : uint256) returns (uint256) { 
  return x * pow10u(18); 
}

// --- Typed smart constructors
function weis(x : uint256) returns (Qty<Wei>) { 
  return Qty(x); 
}

function kweis(x : uint256) returns (Qty<Kwei>) { 
  return Qty(x); 
}

function mweis(x : uint256) returns (Qty<Mwei>) { 
  return Qty(x); 
}

function gweis(x : uint256) returns (Qty<Gwei>) { 
  return Qty(x); 
}

function szabos(x : uint256) returns (Qty<Szabo>) { 
  return Qty(x); 
}

function finneys(x : uint256) returns (Qty<Finney>) { 
  return Qty(x); 
}

function ethers(x : uint256) returns (Qty<Ether>) { 
  return Qty(x); 
}
