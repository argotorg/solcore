# Modules

Every SAIL source file is a module. A module can import definitions from other
modules and control which of its own definitions are visible to importers
through export declarations. The module system provides explicit namespace
management: a name imported from another module is not automatically available
without qualification unless the import form places it directly into scope.

---

## File Layout

A source file is a sequence of import declarations and top-level declarations
in any order. Import declarations may appear before, after, or interleaved with
top-level declarations.

```
( import-declaration | top-level-declaration )*
```

The conventional style places all imports at the top of the file, but this is
not enforced by the compiler.

---

## Import Forms

SAIL provides four import forms. Each form controls how the imported names are
placed into scope and whether they require qualification to use.

### Full module import

```solidity
import modname;
```

Loads the module and makes all its exported names available under the
qualified prefix `modname`. No names are introduced into the unqualified
scope.

```solidity
// token.solc exports: Token, transfer
import token;

function doTransfer(t : token.Token, to : word, amount : word) -> () {
    return token.transfer(t, to, amount);
}
```

### Module import with alias

```solidity
import modname as Alias;
```

Same as a full import but assigns a shorter alias to the module. All
qualified references must use the alias; the original module name is not
available as a qualifier.

```solidity
import token as T;

function doTransfer(t : T.Token, to : word, amount : word) -> () {
    return T.transfer(t, to, amount);
}
```

After this import, writing `token.transfer(t, to, amount)` is an error because `token`
is not a known qualifier in this file.

### Selective import

```solidity
import modname.{Name1, Name2};
```

Loads the listed names directly into the unqualified scope. They can be used
without any prefix.

```solidity
import token.{Token, transfer};

function doTransfer(t : Token, to : word, amount : word) -> () {
    return transfer(t, to, amount);
}
```

Each item in the selector list may optionally be renamed with `as`:

```solidity
import selectlib.{keep as keep_, drop as drop_};

function main(x : word) -> word {
    return drop_(keep_(x));
}
```

The original name `keep` is not placed into scope. Only the alias `keep_` is available.
Multiple items may be renamed independently in the same selector list.

Constructors of an imported type must still be qualified with the type name
even when the type itself was selectively imported:

```solidity
import token.{Token};

function makeActive() -> Token {
    return Token.Active;    // correct
}

// Error: unqualified constructor.
function makeBad() -> Token {
    return Active;
}
```

```
Unqualified constructor:
Active
Use Type.Constructor form.
```

### Wildcard selective import

```solidity
import modname.{*};
```

Places every exported name from the module into the unqualified scope.
Individual names may be excluded using `hiding`:

```solidity
import globlib.{*} hiding {idWord};

function main(x : word) -> word {
    let y : T = mkT(x);    // mkT is in scope; idWord is not
    match y {
    | T.T(v) => return v;
    }
}
```

The `hiding` clause accepts a comma-separated list of names to suppress:

```solidity
import selectlib.{keep, drop} hiding {drop};

function main(x : word) -> word {
    return keep(x);    // drop is not in scope
}
```

### Importing operator symbols

User-defined operator symbols are exported and imported using parenthesised
syntax `(sym)`, where `sym` is the operator character sequence. Importing an
operator brings both its precedence/fixity declaration and its bound function
into scope.

```solidity
// math.solc  (declares and exports the operator)
infixl 70 (^^) => pow;
export { pow, (^^) };

function pow(b : word, e : word) -> word { ... }
```

```solidity
// main.solc  (imports the operator by symbol)
import math.{pow, (^^)};

contract Main {
    function main() -> word {
        return 2 ^^ 10;    // 1024
    }
}
```

Operator symbols may be mixed freely with ordinary names in the same selector
list. Importing the function name alone (without the symbol) makes the
implementation callable as a regular function but does not enable infix syntax.

---

## Module Paths

A module path identifies the source file of a module relative to a root
directory. Dots in the path correspond to directory separators.

### Relative paths

A name without a `lib.` prefix is a _relative path_. The compiler resolves
it relative to the directory that contains the importing file.

```solidity
import foo.bar;          // loads foo/bar.solc
import foo.bar.baz;      // loads foo/bar/baz.solc
```

After a plain `import foo.bar`, the module is accessible under the full
dotted qualifier:

```solidity
import foo.bar;

function main() -> word {
    return foo.bar.value();
}
```

### Library paths

A path that begins with `lib.` is treated as an _absolute library path_,
resolved from the root of the current library rather than the current
directory.

```solidity
export lib.some.module;          // re-exports some/module.solc from the library root
```

Library paths are mainly used in re-export declarations to expose a module
from a different directory tree. They are not commonly used in import
statements directly.

### External library paths

A path that begins with `@libname.` refers to a module in a separately
configured external library root. External libraries are registered in the
build configuration; the compiler resolves them to absolute paths at build
time.

```solidity
import @extlib.math.api;

contract External {
    function main() -> word {
        return math.api.sum(39);
    }
}
```

An alias keeps the reference concise:

```solidity
import @extlib.math.api as MathApi;

function main() -> word {
    return MathApi.sum(39);
}
```

### Standard library

The name `std` and any name that begins with `std.` are resolved to the
standard library. The standard library root is configured separately from
user libraries.

```solidity
import std;

function main() -> word {
    return std.addWord(21, 21);
}
```

---

## Qualified Names

When a module is imported with a full or aliased import, its exported
definitions are accessed through a dotted qualifier. The qualifier may prefix
types, functions, and constructors.

### Qualified type names

```solidity
import token;

function doTransfer(t : token.Token, to : word, amount : word) -> () {
    return token.transfer(t, to, amount);
}
```

### Qualified constructor expressions

Constructors are written as `qualifier.TypeName.Constructor`:

```solidity
import token;

function makeActive() -> token.Token {
    return token.Token.Active;
}
```

### Qualified constructor patterns

The same qualified form is used in pattern matching:

```solidity
import token;

function isActive(t : token.Token) -> word {
    match t {
    | token.Token.Active => return 1;
    | token.Token.Paused => return 0;
    }
}
```

### Qualified names with aliases

When the import carries an alias, replace the module name with the alias
in all qualified references:

```solidity
import token as T;

function makeActive() -> T.Token {
    return T.Token.Active;
}
```

---

## Export Declarations

An export declaration controls which definitions an importing module can see.
Definitions that are not listed in an export declaration are private to the
file.

> **Note** A file without any export declaration exports nothing. All
> definitions are private unless explicitly exported.

### Explicit export list

```solidity
export { Name1, Name2, TypeName };
```

Names are listed by their unqualified identifier.

```solidity
export { Bool, not, C, D, id };
```

### Exporting a type with constructors

By default, exporting a type name makes the type visible but keeps its
constructors private. An importer can use the type in signatures but cannot
construct or pattern-match on its values.

To export constructors explicitly, list them in parentheses after the type
name:

```solidity
export { Token(Ok) };           // exports only the Ok constructor
export { Token(Ok, Err) };      // exports both constructors
export { Bool(*) };             // exports Bool and all its constructors
```

### Exporting operator symbols

Operator symbols are separate exportable entities from the functions they
implement. To make an operator available to importers as infix syntax, both
the function name and the operator symbol must appear in the export list:

```solidity
infixl 70 (^^) => pow;
export { pow, (^^) };
```

Exporting only `pow` (without `(^^)`) makes the function callable by name
but does not grant importers access to the infix `^^` syntax.

### Wildcard export

```solidity
export { * };
```

Exports everything defined in the file. Constructors of all types are also
exported.

### Re-exporting another module

A module may forward its imports so that importers receive them as if they
came from the re-exporting module.

**Re-export a whole module:**

```solidity
// api.solc: makes all of util's exports available under api.util.*
export lib.reexport_module.pkg.util;
```

An importer of `api` then accesses the re-exported names through the full
chain:

```solidity
import reexport_module.pkg.api;

function main() -> word {
    return api.util.unwrap(api.util.Wrap.Mk(1));
}
```

**Re-export a module under an alias:**

```solidity
// api_alias.solc
export lib.reexport_module.pkg.util as Utils;
```

```solidity
import reexport_module.pkg.api_alias;

function main() -> word {
    return api_alias.Utils.unwrap(api_alias.Utils.Wrap.Mk(1));
}
```

**Re-export selected names from a module:**

```solidity
import hidden_ctor_lib;

export hidden_ctor_lib.{Token(Ok)};    // re-exports Token type with Ok constructor only
export hidden_ctor_lib.{mkErr};        // re-exports only the mkErr function
```

---

## Hidden Constructors

When a constructor is not exported, importers receive an _opaque_ type: they
can name the type and pass values around, but they cannot construct new values
directly or inspect existing ones through pattern matching. The only way to
create or examine values of an opaque type is through the functions the module
chooses to export.

```solidity
// hidden_ctor_lib.solc
export {Token(Ok), mkOk, mkErr};

data Token = Ok(word) | Err(word);

function mkOk(x : word) -> Token { return Token.Ok(x); }
function mkErr(x : word) -> Token { return Token.Err(x); }
```

The module exports `Token` with only the `Ok` constructor visible. The `Err`
constructor is private.

An importer that selects only the type cannot use the hidden constructor:

```solidity
import hidden_ctor_lib.{Token};

// Error: Err is not exported.
function bad() -> Token {
    return .Err(1);
}
```

```
No matching constructor for shorthand expression:
.Err
```

Pattern matching on the hidden constructor is equally rejected:

```solidity
import hidden_ctor_lib.{Token, mkErr};

// Error: Token.Err is not in scope.
function bad(x : word) -> word {
    match mkErr(x) {
    | Token.Err(v) => return v;
    | _ => return 0;
    }
}
```

```
Undefined name: Token.Err
```

---

## Transitive Imports

Importing a module does not automatically make its own imports visible. If
module `A` imports module `B`, and module `C` imports module `A`, then `C`
sees only the names that `A` chose to export. Names that `B` exported to `A`
but that `A` did not re-export are not visible in `C`.

```solidity
// transitive_dep_base.solc
export { g };
function g() -> word { return 1; }

// transitive_dep_mid.solc
import transitive_dep_base.{g};
export { f };
function f() -> word { return g(); }

// transitive_dep_main_select.solc
import transitive_dep_mid.{f};
function main() -> word { return f(); }    // g is not in scope here
```

---

## Name Shadowing

A locally defined function or parameter shadows an imported name of the same
identifier. The imported name remains accessible through its qualified form.

```solidity
import token;

// Local 'transfer' shadows token.transfer for unqualified calls.
function transfer(to : word, amount : word) -> () { return (); }

function main(to : word, amount : word) -> () {
    return token.transfer(to, amount);    // uses the imported transfer, not the local one
}
```

A locally defined name also shadows a selectively imported name:

```solidity
import erc20lib.{balanceOf};

// Local 'balanceOf' shadows the imported one.
function balanceOf(account : word) -> word { return 0; }

function main(account : word) -> word {
    return balanceOf(account);    // calls the local balanceOf
}
```

---

## Common Errors

### Using an unqualified name after a full import

A full import (`import modname;`) requires all names to be qualified. Using
an imported name without the qualifier is an error:

```solidity
import token;

// Error: 'transfer' is not in scope unqualified.
function main(to : word, amount : word) -> () {
    return transfer(to, amount);
}
```

```
Undefined name: transfer
```

The fix is to qualify the call: `return token.transfer(to, amount);`.

### Using the original name after aliasing

When an alias replaces the module name, the original name is not a valid
qualifier:

```solidity
import erc20.token as T;

// Error: erc20 is not a qualifier in this file.
function main(to : word, amount : word) -> () {
    return erc20.token.transfer(to, amount);
}
```

```
Undefined name: erc20
```

The fix is to use the alias: `return T.transfer(to, amount);`.

### Using a type without qualifying its constructor

Selectively importing a type name does not bring its constructors into the
unqualified scope. Constructors must always be written as `TypeName.Constructor`:

```solidity
import token.{Token};

// Error: unqualified constructor.
function makeActive() -> Token {
    return Active;
}
```

```
Unqualified constructor:
Active
Use Type.Constructor form.
```

### Using a type not in scope at all

Importing a module without qualification and then using a name without the
module qualifier fails:

```solidity
import token;

// Error: Token is not in the unqualified scope.
function bad(t : Token) -> word {
    return 0;
}
```

```
Undefined type constructor:
Token
```

Use `token.Token`, or switch to a selective import.
