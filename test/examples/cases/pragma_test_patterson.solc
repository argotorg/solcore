// Simple Patterson test - should fail without pragma

forall a . class a:C1 {}
forall a . class a:C2 {}

data T(x) = T;

// This violates Patterson: context measure (2) >= conclusion measure (2)
forall U . U:C1, U:C2 => instance T(U):C1 {}