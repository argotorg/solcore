trait Foo<a, b> {}

trait C<a> {}

impl<t> C<(word, t)> where t: Foo {}
