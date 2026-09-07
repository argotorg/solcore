enum Bool { False, True }
enum W { W(Word) }
enum U { U }

// empty class needed since forall expects a nonempty context
trait Top<a> {}
impl Top<a> {}

/* For experiments, special handling when emitting code */
// this does not work, typechecker forces a ~ b
// forall a, b.(a:Top, b:Top) => function ereturn(x:a) -> b { let res: b; return res; }
// we might have
// forall a.(a:Top) => function ereturn(x:a) -> a
// or

function ereturn<a>(x:a) returns (Unit)  where a: Top { let res: Unit; return res; }
// and then cast it to any type using unsafeCast

/* simulate match expression
  x = match { | Bool.False => return 77; | Bool.True => W(22) }
*/
function elimBool1(b:Bool) returns (Word) {
   let x : W;
   x = W(1);
   match (b ) {
      // this works
      // | Bool.False => x = unsafeCast(ereturn(77));
      // but this does not - unknown intermediate type
      // | Bool.False => x = unsafeCast(unsafeCast(ereturn(77)));
      // what about "return(return 77)"?
      // this works
         case Bool.False { x = unsafeCast(ereturn(ereturn(77)));
      // but this does not
      //   | Bool.False => x = unsafeCast(ereturn(unsafeCast(ereturn(77))));
      } case Bool.True  { x = W(22);
   } }

   match (x ) {
     case W(y) { return y;
   } }

}

// "semicolon"
function semi<a>(x:a) returns (U)  where a: Top { return U;}

function unsafeCast<a, b>(x:a) returns (b)  where a: Top, b: Top {
  let res: b; return res;
}


contract ExpReturn {



  function main() public returns (Word) {
    return elimBool1(Bool.False);
    // return elimBool1(Bool.False);
  }
}
