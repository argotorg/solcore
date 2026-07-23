function lambdaimpl1 (x) {
     return x;
}
enum LambdaTy0<a> { LambdaTy0 }
trait invokable<self, args, ret> {
   function invoke (self : self, args : args) returns (ret);
}
impl invokable<LambdaTy0<a>, a, a> {
   function invoke<a> (self : LambdaTy0<a>, args : a) returns (a) {
    return lambdaimpl1(args);
  }
}
contract SimpleLambda {
   function f () public {
      let n = LambdaTy0 ;
      return invokable.invoke(n, 0);
   }
}
