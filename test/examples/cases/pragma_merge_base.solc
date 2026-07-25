// Test base file for pragma merging functionality
// This file contains violations of all three condition types with pragmas to disable checks

// Pragmas to disable checks for specific classes
pragma solcore noPattersonCondition TestClassP1, TestClassB1, TestClassP3, TestClassB3;
pragma solcore noCoverageCondition TestClassC1, TestClassP3;
pragma solcore noBoundVariableCondition TestClassB1, TestClassB3;

// --- Test Classes ---

trait TestClassP1<a> {}
trait TestClassP2<a> {}
trait TestClassP3<a, b> {}

trait TestClassC1<a, b> {}
trait TestClassC2<a, b, c> {}

trait TestClassB1<a, b> {}
trait TestClassB2<a, b> {}
trait TestClassB3<a> {}

// --- Data Types ---

enum TestType1<x> { TestType1 }
enum TestType2 { TestType2 }

// Fails Patterson: context constraint not smaller then head
impl<U> TestClassP1<U> where (U, word): TestClassP1 {}

// Patterson OK: No context predicates
impl TestClassP2<TestType2> {}

// --- Coverage Condition ---

// Fails Coverage: Variable 'a' only appears in weak position (parameter to TestClassC1)
impl<a, b> TestClassC1<TestType1<b>, a> {}

// Coverage OK: All variables in strong positions
impl TestClassC2<TestType2, TestType2, TestType2> {}

// === Bound Variable Violations ===

// Fails Bound Variable & Patterson: Variable 'c' appears in context but not in instance head
impl<a, c> TestClassB1<TestType1<a>, a> where c: TestClassB2<a> {}

// Bound Variable OK: Simple instance without context
impl TestClassB2<TestType1<TestType2>, TestType2> {}
