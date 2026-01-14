// Verification file for pragma merging
// This file imports pragma_merge_base but has no pragmas of its own
// Tests that pragmas from imported files are properly inherited

import pragma_merge_base;

data VerifyType(x) = VerifyType;

// Would fail without imported pragma no-patterson-condition TestClassP3
forall a . (a,word):TestClassP3(a) => instance a:TestClassP3(word) {}

// Would fail without imported pragma no-coverage-condition TestClassC1
forall p q . instance VerifyType(p):TestClassC1(q) {}
