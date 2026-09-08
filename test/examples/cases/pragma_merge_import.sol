// Test import file for pragma merging functionality
// This file imports pragma_merge_base and adds its own violations with pragmas
// Testing that pragmas from both files are properly merged

import pragma_merge_base;

// Add more pragmas - these should merge with imported ones

trait TestClassC3<a, b> {}
trait TestClassB4<a> {}

// fails coverage & patterson (pragma set here)
impl<i, j> TestClassC3<i, j> where (i, j): TestClassP1 {}

// fails coverage & patterson (pragma set in base)
impl<i, j> TestClassP3<i, j> where (i, j): TestClassP1 {}

// fails bound var & patterson (pragma set here)
impl<a, c> TestClassB4<TestType1<a>> where c: TestClassB1<a> {}

// fails bound var & patterson (pragma set in base)
impl<a, c> TestClassB3<TestType1<a>> where c: TestClassB1<a> {}
