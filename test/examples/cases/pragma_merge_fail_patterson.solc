// This file should FAIL compilation to demonstrate that checks are working when the imported file contains violations

import pragma_merge_base;


// --- Patterson Violation ---

forall a . class a:TestFailClass {}

// Should fail because TestFailClass doesn't have no-patterson-condition
forall U . U:TestClassP1, U:TestClassP2, U:TestClassP3 => instance U:TestFailClass {}
