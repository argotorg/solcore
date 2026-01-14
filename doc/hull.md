# Hull language (low level)

## Introduction
The Hull language is an intermediate step before Yul generation.
It is basically Yul with algebraic types (sums/products)

### Naming

Hull was initially named Core, but once it was decided that the whole language should be called "Core Solidity",
it was renamed to Hull.

## Abstract syntax

See `src/Language/Hull.hs`

### Types

``` haskell
data Type
    = TWord
    | TBool
    | TPair Type Type     -- binary product, e.g. (word * word)
    | TSum Type Type      -- binary sum, e.g. (unit + word)
    | TSumN [Type]        -- n-ary sum
    | TFun [Type] Type
    | TUnit
    | TNamed String Type  -- named type, e.g. Option{unit + word}
```

### Expressions

``` haskell
data Expr
    = EWord Integer
    | EBool Bool
    | EVar Name
    | EPair Expr Expr
    | EFst Expr
    | ESnd Expr
    | EInl Type Expr
    | EInr Type Expr
    | EInK Int Type Expr
    | ECall Name [Expr]
    | EUnit
```

Notes:
- `EInl/EInr` are binary sum constructors (injections)
- `EInK k` represents k-th injection for n-ary sum
- all injections are annotated with target type, so we have for example  `inr<Option{unit + word}>(42)`
- n-ary products not implemented yet, but planned (they are simpler than sums)

### Statements

```
data Stmt
    = SAssign Expr Expr
    | SAlloc Name Type
    | SExpr Expr
    | SAssembly [YulStmt]
    | SReturn Expr
    | SComment String
    | SBlock [Stmt]
    | SMatch Type Expr [Alt]
    | SFunction Name [Arg] Type [Stmt]
    | SRevert String

data Arg = TArg Name Type
data Alt = Alt Con Name Stmt
data Con = CInl | CInr | CInK Int
```

Notes:
- there are no control constructs yet, except call/return/revert; this is mostly because the surface language does not have them either
- using comments can lead to problems, since Solidity seems to have no nested comments
- for `YulStmt`, see `src/Language/Yul.hs`
- as in Yul, there are no statement separators

### Contracts

Right now a contract consists of a name and a list of statements. This is definitely a temporary solution.

```
data Contract = Contract { ccName :: Name, ccStmts ::  [Stmt] }
```


## Concrete Syntax

See `src/Language/Hull/Parser.hs`

Although Hull is not meant to be written manually, it has concrete syntax, mostly for diagnostic purposes.

```
Block = "{" hullStmt* "}"
Contract = "contract" identifier
Stmt = "let" identifier ":" Type
     | "return" Expr
     | match "<" Type ">" Expr "with" "{" Alt* "}"
     | "function" identifier "(" Args? ")" "->" Type Block
     | "assembly" YulBlock
     | "revert" StringLiteral
     | Expr ":=" Expr
     | Expr

Args = identifier : Type ("," Args)?
Alt  = Con identifier "=>" Stmt
Con  = "inl" | "inr" | "in" "(" integer ")"

Expr = Con "<" Type ">" PrimaryExpr
     | Project PrimaryExpr
     | PrimaryExpr

PrimaryExpr
     = integer
     | "true"
     | "false"
     | Tuple
     | identifier "(" ExprList ")"
     | identifier
Tuple = "(" ExprList ")"
ExprList = (Expr (, Expr)*)?
```

### Examples

#### Option

```
contract Option {
    function main () -> word {
      return maybe$Word(0, inr<Option{unit + word}>(42))
    }
    function maybe$Word (n : word, o : Option{unit + word}) -> word {
      match<Option{unit + word}> o with {
        inl $alt => { // None
                      return n
                    }
        inr $alt => { // Some[var_1]
                      let var_2 : word
                      var_2 := $alt
                      return var_2
                    }
      }
    }
}
```

#### Enumeration type
```
contract RGB {
    function fromEnum (c : Color{unit + (unit + unit)}) -> word {
      // match c with
      match<Color{unit + (unit + unit)}> c with {
        inl $alt => {   // R
                        return 4
                    }
        inr $alt => match<unit+unit> $alt with {
                      inl $alt => {   // G
                                      return 2
                                  }
                      inr $alt => {   // B
                                      return 42
                                  }
                    }
      }
    }
    function main () -> word {
      return fromEnum(inr<Color{unit + (unit + unit)}>(inr<unit + unit>(())))
    }
}
```


## Problems

Type annotation are helpful for code generation and seem to be necessary for compression, but they lead to rather verbose syntax, e.g.

```
data Food = Curry | Beans | Other
data CFood = Red[Food] | Green[Food] | Nocolor

  function fromEnum(x : Food) {
     match x {
       | Curry => return 1;
       | Beans => return 2;
       | Other => return 3;
     };
  }


contract Food {
  function eat(x) {
    match x {
       | Red[f] => return f;
       | Green[f] => return f;
       | _ => Other;
    };
  }

  function main() {
  return fromEnum(eat(Green[Beans]));
  }
}
```

yields the following Hull:

```
contract Food {
    function eat (x : CFood{(Food{(unit + (unit + unit))} + (Food{(unit + (unit + unit))} + unit))}) -> Food{(unit + (unit + unit))} {
      match<CFood{(Food{(unit + (unit + unit))} + (Food{(unit + (unit + unit))} + unit))}> x with {
        inl $alt => { // Red[var_8]
                      let var_9 : Food{(unit + (unit + unit))}
                      var_9 := $alt
                      return var_9
                    }
        inr $alt => match<(Food{(unit + (unit + unit))} + unit)> $alt with {
                      inl $alt => { // Green[var_5]
                                    let var_6 : Food{(unit + (unit + unit))}
                                    var_6 := $alt
                                    return var_6
                                  }
                      inr $alt => revert "no match for: Nocolor"
                    }
      }
    }
    function fromEnum (x : Food{(unit + (unit + unit))}) -> word {
      match<Food{(unit + (unit + unit))}> x with {
        inl $alt => { // Curry
                      return 1
                    }
        inr $alt => match<(unit + unit)> $alt with {
                      inl $alt => { // Beans
                                    return 2
                                  }
                      inr $alt => { // Other
                                    return 3
                                  }
                    }
      }
    }
    function main () -> word {
      return fromEnum(eat(inr<CFood{(Food{(unit + (unit + unit))} + (Food{(unit + (unit + unit))} + unit))}>(inl<(Food{(unit + (unit + unit))} + unit)>(inr<Food{(unit + (unit + unit))}>(inl<(unit + unit)>(()))))))
    }
}
```

On the other programs are not really meant to be read by humans.