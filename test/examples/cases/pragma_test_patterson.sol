// Simple Patterson test - should fail without pragma

trait C1<a> {}
trait C2<a> {}

enum T<x> { T }

// This violates Patterson: context measure (2) >= conclusion measure (2)
impl<U> C1<T<U>> where U: C1, U: C2 {}