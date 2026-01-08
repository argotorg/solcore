// Same test but with pragma to disable bound variable check
// This SHOULD PASS

pragma no-bounded-variable-condition TestBound;
pragma no-patterson-condition TestBound; // Also disable Patterson to avoid that error

forall a . class a:TestBound {}
forall a b . class a:TestHelper(b) {}

data TestType(x) = TestType;

// Variable 'bad' appears in context but not in instance head
// But pragma disables the check, so should pass
forall x bad . bad:TestHelper(x) => instance TestType(x):TestBound {}
