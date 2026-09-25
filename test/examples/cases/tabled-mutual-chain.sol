enum WrapA<a> { WrapA(a) }
enum WrapB<a> { WrapB(a) }

trait A<a> {}
trait B<a> {}

impl A<word> {}

impl<a> B<WrapB<a>> where a: A {}
impl<a> A<WrapA<a>> where a: B {}

function needsA<a>(x:a) returns (unit)  where a: A {
  return;
}

function main() returns (unit) {
  return needsA(WrapA(WrapB(0)));
}
