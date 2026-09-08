// Verification file for pragma merging
// This file imports pragma_merge_base but has no pragmas of its own
// Tests that pragmas from imported files are properly inherited

import pragma_merge_base;

enum VerifyType<x> { VerifyType }

// Would fail without imported pragma no-patterson-condition TestClassP3
impl<a> TestClassP3<a, word> where (a, word): TestClassP3<a> {}

// Would fail without imported pragma no-coverage-condition TestClassC1
impl<p, q> TestClassC1<VerifyType<p>, q> {}
