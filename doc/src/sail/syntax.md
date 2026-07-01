# Syntax

This page is a complete grammar reference for SAIL. Every grammar rule appears
as its own section, followed by a railroad diagram. Rounded boxes denote
terminal tokens (keywords and punctuation); rectangular boxes denote
non-terminals and link to the corresponding rule. The notation follows standard
EBNF conventions: `[ … ]` marks optional elements and `{ … }` marks
zero-or-more repetition.

---

## Parser Rules

### CompilationUnit

A SAIL source file is a sequence of import declarations and top-level
declarations in any order. Imports are not required to precede declarations.

![CompilationUnit](diagrams/CompilationUnit.svg)

---

### TopDecl

A top-level declaration is one of: a contract, a free function, a type class,
an instance, an algebraic data type, a type synonym, an export declaration, a
pragma, or an operator declaration.

![TopDecl](diagrams/TopDecl.svg)

---

### Import

An import declaration makes names from another module available in the current
module. The `@package.` prefix selects an external package; the `lib.` prefix
selects a standard library module.

![Import](diagrams/Import.svg)

---

### ModulePath

A module path is a dot-separated sequence of identifiers that locates a module
within a package.

![ModulePath](diagrams/ModulePath.svg)

---

### ImportItems

The list of names to import from a module, enclosed in braces.

![ImportItems](diagrams/ImportItems.svg)

---

### ImportItem

A single item to import. Four forms are available:

- `*` imports all exported names (wildcard).
- `Name` imports a single name into the unqualified scope.
- `Name as Alias` imports a single name under a local alias. The original name is not placed in scope.
- `(sym)` imports an operator symbol and enables its infix syntax in this file.

![ImportItem](diagrams/ImportItem.svg)

---

### OperatorSymbol

A non-empty sequence of operator characters (`+-*/%<>=!&|^~#?` or Unicode
mathematical symbols in the range U+2200 to U+23FF), written between
parentheses in import and export item lists.

![OperatorSymbol](diagrams/OperatorSymbol.svg)

---

### HidingList

A comma-separated list of names to exclude from an import.

![HidingList](diagrams/HidingList.svg)

---

### ExportDecl

An export declaration controls which names this module exposes to other modules.

![ExportDecl](diagrams/ExportDecl.svg)

---

### ExportItems

The list of names to export from the current module, enclosed in braces.

![ExportItems](diagrams/ExportItems.svg)

---

### ExportItem

A single export entry. It may be the wildcard `*`, a plain name, a name together
with its constructors, all names from a module, or an operator symbol in
parentheses.

![ExportItem](diagrams/ExportItem.svg)

---

### ExportConstructors

Selects which data constructors to re-export alongside a type name: either all
constructors (`*`) or an explicit list.

![ExportConstructors](diagrams/ExportConstructors.svg)

---

### ExportFromItems

The list of names re-exported from another module.

![ExportFromItems](diagrams/ExportFromItems.svg)

---

### ExportFromItem

A single entry in a re-export list: the wildcard `*`, a plain name, or a name
with explicit constructor re-exports.

![ExportFromItem](diagrams/ExportFromItem.svg)

---

### Pragma

A pragma adjusts compiler behaviour for type class constraint checking. Without
targets the pragma applies to all classes; with a list of identifiers it applies
only to those specific classes.

![Pragma](diagrams/Pragma.svg)

---

### PragmaKind

The three available pragma kinds relax, respectively, the coverage condition,
the Patterson condition, and the bound-variable condition for type class
instance resolution.

![PragmaKind](diagrams/PragmaKind.svg)

---

### PragmaTargets

A comma-separated list of class names to which a pragma applies.

![PragmaTargets](diagrams/PragmaTargets.svg)

---

### Type

A type is one of: a named type constructor applied to zero or more type
arguments, a function type of the form `(T₁, …, Tₙ) -> T`, a tuple or unit
type written as a parenthesised comma-separated list, or a proxy type `@T`.

![Type](diagrams/Type.svg)

---

### TypeList

A comma-separated (possibly empty) list of types, used as arguments to type
constructors and as the parameter list of function types.

![TypeList](diagrams/TypeList.svg)

---

### TypeVarSeq

A space-separated sequence of type-variable names following a `forall` keyword.
All listed names are universally quantified over the scope of the accompanying
signature.

![TypeVarSeq](diagrams/TypeVarSeq.svg)

---

### TypeVarParams

A comma-separated list of type-variable names enclosed in parentheses. Used in
`data`, `type`, and `class` declarations to introduce parametric type
arguments.

![TypeVarParams](diagrams/TypeVarParams.svg)

---

### TypeName

A possibly qualified type name. Simple names are single identifiers; qualified
names chain module components with `.`.

![TypeName](diagrams/TypeName.svg)

---

### DataDef

An algebraic data type declaration. The optional parameter list introduces
type variables. The optional body lists the constructors separated by `|`.

![DataDef](diagrams/DataDef.svg)

---

### DataConstrs

One or more data constructor definitions separated by `|`.

![DataConstrs](diagrams/DataConstrs.svg)

---

### DataConstr

A single data constructor: a name optionally followed by a
parenthesised, comma-separated list of field types.

![DataConstr](diagrams/DataConstr.svg)

---

### TypeSynonym

A type synonym introduces an alias for an existing type. The optional parameter
list introduces type variables that may appear in the right-hand side.

![TypeSynonym](diagrams/TypeSynonym.svg)

---

### Pattern

A pattern appears in `match` equations to deconstruct a value by its
constructor. The dot-prefix form (`.Name`) is a contextual shorthand: the
constructor is resolved from the type being matched.

![Pattern](diagrams/Pattern.svg)

---

### PatternList

A comma-separated list of patterns used as the argument list of a constructor
pattern or as the simultaneous arguments of a `match` equation.

![PatternList](diagrams/PatternList.svg)

---

### Expr

An expression computes a value. Binary operators follow standard precedence:
arithmetic binds tighter than comparison, which binds tighter than logical. All
binary operators are left-associative except `if-then-else`, which is
right-associative.

![Expr](diagrams/Expr.svg)

---

### ExprList

A comma-separated (possibly empty) list of expressions used as function
arguments.

![ExprList](diagrams/ExprList.svg)

---

### Literal

A literal value: a decimal or hexadecimal integer, or a double-quoted string.

![Literal](diagrams/Literal.svg)

---

### Stmt

A statement is an executable step inside a function body. Assignment operators
`=`, `+=`, and `-=` require a terminating `;`. `let` declares a local variable,
optionally with a type annotation and an initialiser. The `for` statement provides
a C-style counted loop; its initialisation and post-iteration clauses obey the
`ForInitStmt` and `ForPostStmt` grammars respectively.

![Stmt](diagrams/Stmt.svg)

---

### Body

A brace-enclosed sequence of zero or more statements forming the body of a
function, branch, or constructor.

![Body](diagrams/Body.svg)

---

### ForInitStmt

The initialisation clause of a `for` loop. It may be an assignment, a compound
assignment, a `let` binding (typed or untyped, with or without an initialiser),
or a plain expression. Unlike a regular statement, there is no trailing `;` — the
semicolons are written explicitly in the `for(…; …; …)` header.

![ForInitStmt](diagrams/ForInitStmt.svg)

---

### ForPostStmt

The post-iteration clause executed after each loop body. It follows the same
grammar as `ForInitStmt`. A `let` binding introduced here is scoped to the body
of that single iteration.

![ForPostStmt](diagrams/ForPostStmt.svg)

---

### MatchArgs

One or more comma-separated expressions forming the scrutinees of a `match`
statement.

![MatchArgs](diagrams/MatchArgs.svg)

---

### Equation

A single match arm: a `|`-prefixed list of patterns followed by `=>` and a
sequence of statements. The patterns are matched positionally against the
scrutinee list.

![Equation](diagrams/Equation.svg)

---

### Param

A single function parameter: a name with an explicit type annotation, or an
untyped name whose type will be inferred.

![Param](diagrams/Param.svg)

---

### ParamList

A comma-separated list of function parameters.

![ParamList](diagrams/ParamList.svg)

---

### Function

A function definition. The long form uses a brace-enclosed statement block as
the body. The short form uses a single expression whose value is returned
implicitly (Rust-style).

![Function](diagrams/Function.svg)

---

### Signature

A function signature declares the function name, its parameter list, and the
optional return type. It may be preceded by a polymorphism prefix to introduce
type variables and constraints.

![Signature](diagrams/Signature.svg)

---

### SigPrefix

An optional `forall` quantifier that precedes a function or method signature. It
introduces universally quantified type variables and, optionally, a list of type
class constraints that callers must satisfy.

![SigPrefix](diagrams/SigPrefix.svg)

---

### ConstraintList

A comma-separated list of type class constraints.

![ConstraintList](diagrams/ConstraintList.svg)

---

### Constraint

A single type class constraint of the form `Type : ClassName` or
`Type : ClassName(T₁, …, Tₙ)`. It asserts that the given type is an instance
of the named class, possibly with additional type parameters.

![Constraint](diagrams/Constraint.svg)

---

### ClassDef

A type class declaration. The self-variable (the first identifier after
`class`) is the main type being constrained. The optional comma-separated list
in parentheses introduces auxiliary associated type variables. The body lists
method signatures, each terminated by `;`.

![ClassDef](diagrams/ClassDef.svg)

---

### InstDef

An instance declaration provides method implementations for a specific type.
The optional `default` keyword marks the instance as an overlappable fallback
when no more specific instance is found.

![InstDef](diagrams/InstDef.svg)

---

### Contract

A contract groups fields, nested data types, methods, and an optional
constructor. The optional parameter list makes the contract generic over type
variables.

![Contract](diagrams/Contract.svg)

---

### ContractDecl

A single declaration inside a contract body: a field, a data type, a function,
or a constructor.

![ContractDecl](diagrams/ContractDecl.svg)

---

### FieldDecl

A contract field declaration. The type annotation is mandatory; the initialiser
expression is optional.

![FieldDecl](diagrams/FieldDecl.svg)

---

### Constructor

A contract constructor is invoked exactly once at deployment time. It has an
explicit parameter list and a statement block body.

![Constructor](diagrams/Constructor.svg)

---

### AsmBlock

An inline assembly block embeds Yul statements directly in SAIL source code,
giving direct access to EVM opcodes.

![AsmBlock](diagrams/AsmBlock.svg)

---

### YulStmt

A statement in the Yul sublanguage. Yul provides low-level control flow (`if`,
`switch`, `for`, `break`, `continue`, `leave`) and variable declarations and
assignments using `:=`.

![YulStmt](diagrams/YulStmt.svg)

---

### YulCase

A single `case` arm in a Yul `switch` statement: a literal value followed by a
block of Yul statements.

![YulCase](diagrams/YulCase.svg)

---

### YulExpr

A Yul expression: a literal, a variable reference, a function call, or a call
to the special `return` built-in.

![YulExpr](diagrams/YulExpr.svg)

---

### YulNames

A comma-separated list of identifiers used as the left-hand side of a Yul
multi-assignment or the names in a Yul `let` declaration.

![YulNames](diagrams/YulNames.svg)

---

### YulExprList

A comma-separated list of Yul expressions used as arguments to a Yul function
call.

![YulExprList](diagrams/YulExprList.svg)

---

### YulLiteral

A Yul literal value: a decimal or hexadecimal integer, or a string.

![YulLiteral](diagrams/YulLiteral.svg)

---

## Lexer Rules

### Identifier

An identifier begins with a letter (upper or lower case) and may contain
letters, decimal digits, and underscores. Identifiers are used for variable
names, function names, type names, module components, and constructor names.

![Identifier](diagrams/Identifier.svg)

---

### Integer

An integer literal is either a sequence of decimal digits or a hexadecimal
literal prefixed with `0x`.

![Integer](diagrams/Integer.svg)

---

### StringLiteral

A string literal is a sequence of characters enclosed in double quotes.
Supported escape sequences are `\n` (newline), `\t` (tab), and `\"` (literal
double quote).

![StringLiteral](diagrams/StringLiteral.svg)
