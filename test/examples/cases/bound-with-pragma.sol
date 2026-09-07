// Same test but with pragma to disable bound variable check
// This SHOULD PASS

pragma solcore noBoundVariableCondition TestBound;
pragma solcore noPattersonCondition TestBound; // Also disable Patterson to avoid that error

trait TestBound<a> {}
trait TestHelper<a, b> {}

enum TestType<x> { TestType }

// Variable 'bad' appears in context but not in instance head
// But pragma disables the check, so should pass
impl<x, bad> TestBound<TestType<x>> where bad: TestHelper<x> {}
