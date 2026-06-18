pragma no-patterson-condition;
pragma no-bounded-variable-condition;

export { Generic };

import std.{*};

// MPTC: isomorphism between a user type and its SOP representation.
// The representation 'rep' is built from primitive Solcore types:
//   sum(f, g)  with constructors inl / inr
//   (f, g)     pair (product)
//   ()         unit
forall a rep.
class a : Generic(rep) {
    function from(x : a) -> rep;
    function to(x : rep) -> a;
}
