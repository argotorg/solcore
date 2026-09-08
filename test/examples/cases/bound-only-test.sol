// Test only bound variable check, disable Patterson

trait TestBound<a> {}
trait TestHelper<a, b> {}

enum TestType<x> { TestType }

// Variable 'bad' appears in context but not in instance head
// Should fail bound variable check
impl<x> TestBound<TestType<x>> where bad: TestHelper<x> {}
