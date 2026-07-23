enum Bool { False, True }
enum W { W(word) }
enum U { U }


/* For experiments, special handling when emitting code */
// this does not work, typechecker forces a ~ b
// function ereturn(x:a) -> b { let res: b; return res; }
// we might have
// function ereturn(x:a) -> a
// or

function ereturn(x:a) returns (unit) { let res: unit; return res; }
// and then cast it to any type using unsafeCast

/* simulate match expression
  x = match { | Bool.False => return 77; | Bool.True => W(22) }
*/
function elimBool1(b:Bool) returns (word) {
   let x : W;
   x = W(1);
   match (b ) {
      // this works
      case Bool.False { x = unsafeCast(ereturn(77));
      // but this does not - unknown intermediate type
      // | Bool.False => x = unsafeCast(unsafeCast(ereturn(77)));
      // what about "return(return 77)"?
      // this does not work
      // | Bool.False => x = ereturn(ereturn(77));
      // this works
      //  | Bool.False => x = unsafeCast(ereturn(ereturn(77)));
      // this does not work (monomorphisation fails):
      //  | Bool.False => x = unsafeCast(ereturn(unsafeCast(ereturn(77))));

      } case Bool.True  { x = W(22);
   } }

   match (x ) {
     case W(y) { return y;
   } }

}

// "semicolon"
function semi(x:a) returns (U) { return U;}

function unsafeCast(x:a) returns (b) {
let res: b; return res;
}


contract ExpReturn {
  function main() public returns (word) {
    return elimBool1(Bool.False);
    // return elimBool1(Bool.True);
  }
}
