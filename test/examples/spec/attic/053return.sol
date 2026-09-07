enum Bool { False, True }
enum W { W(word) }


/* For experiments, special handling when emitting code */
function ereturn(x:a) returns (b) { let res: b; return res; }

/* simulate match expression
  x = match { | Bool.False => return 77; | Bool.True => W(22) }
*/
function elimBool1(b:Bool) returns (word) {
   let x : W;
   x = W(1);
   match (b ) {
      // this works
      case Bool.False { x = ereturn(77);
      // what about "return(return 77)"?
      // this does not work (monomorphisation fails)
      // | Bool.False => x = ereturn(ereturn(77));

      } case Bool.True  { x = W(22);
   } }

   match (x ) {
     case W(y) { return y;
   } }

}


contract ExpReturn {
  function main() public returns (word) {
    return elimBool1(Bool.False);
    // return elimBool1(Bool.True);
  }
}
