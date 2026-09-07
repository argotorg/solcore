// This file should FAIL compilation to demonstrate that checks are working when the imported file contains violations

import pragma_merge_base;


// --- Patterson Violation ---

trait TestFailClass<a> {}

// Should fail because TestFailClass doesn't have no-patterson-condition
impl<U> TestFailClass<U> where U: TestClassP1, U: TestClassP2, U: TestClassP3 {}
