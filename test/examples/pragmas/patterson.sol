
trait A<self> {}
trait B<self> {}
trait C<self> {}
trait D<self> {}


enum Uint256 { U }
enum T<x> { T }
enum S<x> { SCons }

// This works.
impl<U> D<T<U>> where U: A {}

// This should also work, but reports a violation of the Paterson condition.
impl<U> D<S<U>> where U: A, U: B, U: C {}
