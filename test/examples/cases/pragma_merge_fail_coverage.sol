// Negative test for pragma merging - should fail
import pragma_merge_base;

trait TestFailClass<a> {}

enum FailType<x> { FailType }

// should fail because TestFailCoverage doesn't have no-coverage-condition
trait TestFailCoverage<a, b> {}
impl<x, y> TestFailCoverage<FailType<x>, y> {}
