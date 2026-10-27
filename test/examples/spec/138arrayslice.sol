// Array slices a[start:end] over a memory array, in all four bound forms
// (a[:], a[:end], a[start:], a[start:end]) plus re-slicing. A slice is a
// read-only view whose .length() and s[i] delegate to the base array.
import * from std;
pragma no-patterson-condition ;
pragma no-coverage-condition ;
pragma no-bounded-variable-condition ;

contract ArraySliceSpec {
  function main() returns (uint256) {
    let a : memory<DynArray<uint256>> = [10, 20, 30, 40, 50];
    let full  = a[:];                     // {10,20,30,40,50}
    let front = a[:uint256(2)];           // {10,20}
    let back  = a[uint256(3):];           // {40,50}
    let mid   = a[uint256(1):uint256(4)]; // {20,30,40}
    let inner = mid[uint256(1):];         // {30,40}
    // 5 + 2 + 2 + 3 + 2 lengths, plus mid[0]=20 and inner[1]=40 -> 74
    return full.length() + front.length() + back.length() + mid.length()
         + inner.length() + mid[uint256(0)] + inner[uint256(1)];
  }
}
