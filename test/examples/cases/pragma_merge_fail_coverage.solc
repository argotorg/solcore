// Negative test for pragma merging - should fail
import pragma_merge_base;

forall a . class a:TestFailClass {}

data FailType(x) = FailType;

// should fail because TestFailCoverage doesn't have no-coverage-condition
forall a b . class a:TestFailCoverage(b) {}
forall x y . instance FailType(x):TestFailCoverage(y) {}
