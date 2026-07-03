data WrapA(a) = WrapA(a);
data WrapB(a) = WrapB(a);

forall a . class a:A {}
forall a . class a:B {}

instance word:A {}

forall a . a:A => instance WrapB(a):B {}
forall a . a:B => instance WrapA(a):A {}

forall a . a:A => function needsA(x:a) -> () {
  return ();
}

function main() -> () {
  return needsA(WrapA(WrapB(0)));
}
