# ContractX.hs - Trees That Grow Transformation of Contract Types

## Overview

`ContractX.hs` is a TTG (Trees That Grow) style reimplementation of the types from `Contract.hs`. It extends the AST with type-parameterized extension points, allowing different information to be attached at different compiler passes without duplicating the entire type structure.

This module follows the same patterns established in `StmtX.hs` and coexists with the original `Contract.hs` without replacement.

## File Details

- **Location**: `src/Solcore/Frontend/Syntax/ContractX.hs`
- **Size**: 352 lines
- **Compilation Status**: ✅ Compiles without errors
- **Dependencies**: StmtX, Ty, Contract (for re-exported types)

## Type Transformations

All parameterized types from `Contract.hs` have been transformed to TTG style with phantom type parameter `x`:

### Sum Types (with extension points)

| Original | TTG Version | Extension Type | Constructors |
|----------|-------------|-----------------|--------------|
| `TopDecl a` | `TopDeclX x` | `XXTopDecl x` | 8 (TContrX, TFunDefX, TClassDefX, TInstDefX, TMutualDefX, TDataDefX, TSymX, TPragmaDeclX) |
| `ContractDecl a` | `ContractDeclX x` | `XXContractDecl x` | 5 (CDataDeclX, CFieldDeclX, CFunDeclX, CMutualDeclX, CConstrDeclX) |

Each constructor has an extension field (e.g., `XTContr x`, `XTFunDef x`) as the first parameter.

### Record Types (parameterized)

| Original | TTG Version |
|----------|-------------|
| `CompUnit a` | `CompUnitX x` |
| `Contract a` | `ContractX x` |
| `Field a` | `FieldX x` |
| `Constructor a` | `ConstructorX x` |
| `FunDef a` | `FunDefX x` |
| `Signature a` | `SignatureX x` |
| `Class a` | `ClassX x` |
| `Instance a` | `InstanceX x` |

### Unchanged Types

Types without parameterization are imported and re-exported from `Contract.hs`:
- `Import`, `Pragma`, `PragmaType`, `PragmaStatus`
- `DataTy`, `Constr`
- `TySym`

## Extension Families

Extension families define what information can be attached to each AST node at different compiler passes.

### TopDeclX Extensions (9 families)

```haskell
type family XTContr x      -- Contract declaration
type family XTFunDef x     -- Top-level function definition
type family XTClassDef x   -- Class definition
type family XTInstDef x    -- Instance definition
type family XTMutualDef x  -- Mutual recursion group
type family XTDataDef x    -- Data type definition
type family XTSym x        -- Type synonym definition
type family XTPragmaDecl x -- Pragma directive
type family XXTopDecl x    -- Extension point for future additions
```

### ContractDeclX Extensions (6 families)

```haskell
type family XCDataDecl x   -- Data declaration in contract
type family XCFieldDecl x  -- Field declaration
type family XCFunDecl x    -- Function declaration
type family XCMutualDecl x -- Mutual recursion group
type family XCConstrDecl x -- Constructor declaration
type family XXContractDecl x -- Extension point
```

### Type Instances

All extension families have instances for all compiler passes:

```haskell
type instance XTContr ComPs = NoExtField
type instance XTContr ComNm = NoExtField
type instance XTContr ComTc = NoExtField
type instance XTContr ComSp = NoExtField
-- ... (similar for all extension families)
```

Currently, all instances use `NoExtField` (a unit type), indicating no extension data is needed at any pass. This can be changed in the future to attach pass-specific information.

## Compiler Passes

Four compiler passes are supported (inherited from StmtX):

- **ComPs** - Parsed: Raw syntax tree from parser
- **ComNm** - Named: After name resolution and elaboration
- **ComTc** - Typechecked: After type inference and checking
- **ComSp** - Specialised: After monomorphization and specialization

## Deriving Instances

All types have proper deriving instances for:
- **Eq** - Equality
- **Ord** - Ordering
- **Show** - String representation
- **Data** - Generic programming support
- **Typeable** - Runtime type information

Instance declarations use standalone deriving with `ForallContractX` constraints to properly handle the extension families:

```haskell
deriving instance (ForallContractX Eq x) => Eq (TopDeclX x)
deriving instance (ForallContractX Ord x) => Ord (TopDeclX x)
deriving instance (ForallContractX Show x) => Show (TopDeclX x)
deriving instance (ForallContractX Data x, Typeable x, Data x) => Data (TopDeclX x)
```

## Constraint Helper Type

`ForallContractX` is a constraint type alias that bundles all extension families plus inherited constraints from `ForallStmtX`:

```haskell
type ForallContractX (p :: Type -> Constraint) x =
    ( -- TopDeclX extensions
      p(XTContr x), p(XTFunDef x), p(XTClassDef x), p(XTInstDef x)
    , p(XTMutualDef x), p(XTDataDef x), p(XTSym x), p(XTPragmaDecl x)
    , p(XXTopDecl x)
      -- ContractDeclX extensions
    , p(XCDataDecl x), p(XCFieldDecl x), p(XCFunDecl x)
    , p(XCMutualDecl x), p(XCConstrDecl x), p(XXContractDecl x)
      -- From StmtX (needed for nested types)
    , ForallStmtX p x
    )
```

This allows a single constraint to be applied to all extension points, simplifying deriving declarations and type signatures.

## Type Synonyms

Convenience type aliases for the Named pass (the most common in the current codebase):

```haskell
type NamedCompUnit = CompUnitX ComNm
type NamedTopDecl = TopDeclX ComNm
type NamedContract = ContractX ComNm
type NamedField = FieldX ComNm
type NamedFunDef = FunDefX ComNm
type NamedClass = ClassX ComNm
type NamedInstance = InstanceX ComNm
type NamedSignature = SignatureX ComNm
type NamedConstructor = ConstructorX ComNm
type NamedContractDecl = ContractDeclX ComNm
```

These provide a shorthand for working with named-pass ASTs, which are the most frequently used in practice.

## Pattern Synonyms

Bidirectional pattern synonyms are provided to ease migration from `Contract.hs` to `ContractX.hs` by hiding the `NoExtField` parameters:

### TopDeclX Patterns

```haskell
pattern TContr :: ContractX ComNm -> TopDeclX ComNm
pattern TFunDef :: FunDefX ComNm -> TopDeclX ComNm
pattern TClassDef :: ClassX ComNm -> TopDeclX ComNm
pattern TInstDef :: InstanceX ComNm -> TopDeclX ComNm
pattern TMutualDef :: [TopDeclX ComNm] -> TopDeclX ComNm
pattern TDataDef :: DataTy -> TopDeclX ComNm
pattern TSym :: TySym -> TopDeclX ComNm
pattern TPragmaDecl :: Pragma -> TopDeclX ComNm
```

### ContractDeclX Patterns

```haskell
pattern CDataDecl :: DataTy -> ContractDeclX ComNm
pattern CFieldDecl :: FieldX ComNm -> ContractDeclX ComNm
pattern CFunDecl :: FunDefX ComNm -> ContractDeclX ComNm
pattern CMutualDecl :: [ContractDeclX ComNm] -> ContractDeclX ComNm
pattern CConstrDecl :: ConstructorX ComNm -> ContractDeclX ComNm
```

Both sets of patterns include `COMPLETE` pragmas to guide exhaustiveness checking:

```haskell
{-# COMPLETE TContr, TFunDef, TClassDef, TInstDef, TMutualDef,
             TDataDef, TSym, TPragmaDecl, XTopDeclX #-}

{-# COMPLETE CDataDecl, CFieldDecl, CFunDecl, CMutualDecl,
             CConstrDecl, XContractDeclX #-}
```

When pattern matching in the Named pass, users can omit the extension fields and work directly with the payload types.

## Design Decisions

1. **Name Fields Stay as Name**: Explicit `Name` fields (like `contractName :: Name`) remain as plain `Name`, not parameterized as `XName x`. The parameterization is handled through nested types that use `XName x`.

2. **All Passes Get Extension Instances**: `NoExtField` instances are provided for all four passes (ComPs, ComNm, ComTc, ComSp), not just the commonly used ones. This allows future extension without modifying the type family instances.

3. **Pattern Synonyms for Migration**: Bidirectional patterns with COMPLETE pragmas make it easier to incrementally migrate code from the original `Contract.hs` to `ContractX.hs`.

4. **Nested StmtX Types**: All statement and expression types are imported from `StmtX.hs` (`StmtX`, `ExpX`, `ParamX`, `BodyX`), ensuring consistency with the statement/expression TTG transformation.

## Integration with StmtX

ContractX integrates seamlessly with StmtX:

- Imports TTG statement types: `StmtX`, `ExpX`, `ParamX`, `BodyX`
- Inherits `Pass`, `ComPass`, and the pass singletons from StmtX
- Uses `ForallStmtX` as a component of `ForallContractX`
- Uses `XName` type family from StmtX for all name parameters in nested types

## Integration with Cabal

The module is registered in `sol-core.cabal` under `exposed-modules`:

```
Solcore.Frontend.Syntax.ContractX
```

This allows the TTG types to be used throughout the codebase while the original `Contract.hs` types remain available.

## Current Status

- **Implementation**: Complete
- **Module Compilation**: ✅ No errors (clean compilation with `-fno-code`)
- **Integration**: Not yet integrated into the compiler pipeline
- **Usage**: The module exists alongside `Contract.hs` following the same pattern as `StmtX.hs`

## Future Work

1. **Extension Data**: Add actual extension data for ComTc and ComSp passes (type information, specialization metadata, etc.)
2. **Pipeline Integration**: Integrate ContractX into ElabTree, TcContract, and other pipeline stages
3. **Code Migration**: Gradually migrate existing code from Contract types to ContractX types
4. **Type Removal**: Eventually deprecate and remove the original Contract.hs types

## References

- `StmtX.hs` - TTG implementation for statement and expression types (established pattern)
- `Contract.hs` - Original non-TTG contract types
- Plan document: `/home/ben/.claude/plans/abundant-snacking-rabin.md`
