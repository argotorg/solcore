pragma solcore noCoverageCondition Bar;

enum Wrap<a> { Wrap(a) }

trait Foo<self, rep> {}

trait Bar<self, rep> {}

impl<a, b> Bar<Wrap<a>, b> where a: Foo<b> {}

function need_bar<a, rep>(x : Wrap<a>) returns (())  where Wrap<a>: Bar<rep> {
    return;
}

function use_bar<a>(x : Wrap<a>) returns (())  where a: Foo<word> {
    need_bar(x);
    return;
}
