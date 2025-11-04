# Claude Context: Crisp Type Inference

## Branch Overview

This branch (`polymorphic_type_checking` merged into `claude/analyze-repo-branches-011CUoHw8HJFRdqF1vSEzLmF`) contains a Hindley-Milner-style type inference system for the Crisp programming language. The type system is **partially working** but fails on polymorphic functions.

### Branch History

1. Original `polymorphic_type_checking` branch was created and merged into master (PR #1, July 2020)
2. Type inference was immediately disabled due to polymorphic function issues
3. Two days later, the entire type system was deleted in favor of operator overloading (commit `6635599`)
4. This branch restores the type system and adds:
   - Float support with explicit operators (`+.`, `*.`, `^.` for floats vs `+`, `*`, `^` for ints)
   - Native functions (`i2f`, `f2i`) with type signatures
   - Integer exponentiation optimization

## Current Status

### ✅ Working
- Type inference for simple expressions
- Int, Float, Bool, Unit types
- Tuple, List, Record types
- Function types
- Type unification
- Runtime evaluation (all `eval` tests pass)

### ❌ Failing: Polymorphic Functions

The type checker fails on 7 tests involving polymorphic functions:

```crisp
# Expected: (($A -> $B), $A) -> $B
# Got:      (($A -> $B), $C) -> $D
(f,x) -> f(x)

# Expected: (([$A…], ($A -> $B)) -> [$B…])
# Got:      (([$A…], ($B -> $C)) -> [$D…])
map([h;t], f) -> [f(h) ; map(t,f)]
```

The issue: type variables that should be unified are being assigned independent type variables.

## Type System Architecture

### Key Files

- **`ast/tipe.go`** (959 lines) - Type inference and checking
- **`ast/tipe_test.go`** (247 lines) - Type system tests
- **`ast/ast.go`** - AST node definitions with `FinalTipe()` methods
- **`eval/eval.go`** - Runtime evaluator (dispatches on types for performance)

### Type System Components

#### Types (`Tipe` interface)
- `OmegaTipe` (Ω) - Top type, no information yet
- `EmptyTipe` (∅) - Bottom type, type error
- `SimpleTipe` - Int, Float, Bool, Unit
- `TupleTipe` - Fixed-length heterogeneous: `(Int, Bool, Float)`
- `RecordTipe` - Named fields: `{x: Int, y: Float}`
- `ListTipe` - Variable-length homogeneous: `[Int…]`
- `FuncTipe` - Function: `(Int -> Bool)`

#### Type Variables (`TipeVar`)
- Each expression gets a type variable
- Type variables form a union-find structure
- `ref` points to parent or concrete type
- `ID` used to impose ordering and avoid cycles

#### Type Inference Process

1. **`inferTipes()`** - Traverse AST, assign type variables, gather constraints
   - Creates fresh type variables for each expression
   - Calls `unify()` to integrate type information
   - For function applications: calls `deferUnifyPoly()` to handle later

2. **Polymorphic handling** - Copy function types with fresh variables
   ```go
   for _, funcApplication := range tc.funcApplications {
       polyTipe := derefTipeVar(funcApplication.LExpr.TipeVar(tc))
       domainTV := funcApplication.RExpr.TipeVar(tc)
       rangeTV := funcApplication.TipeVar(tc)

       copyTipe := tc.deepCopyTipe(polyTipe.ref).(*FuncTipe)
       tc.unify(copyTipe.Domain, domainTV)
       tc.unify(copyTipe.Range, rangeTV)
   }
   ```

3. **`finalizeTipes()`** - Dereference all type variables, store final types

4. **Error checking** - Report any expressions with `EmptyTipe`

### The Polymorphic Function Problem

**What should happen:**
```crisp
(f, x) -> f(x)
```
- `f` has type `($A -> $B)`
- `x` has type `$A`
- Result has type `$B`
- Function has type `(($A -> $B), $A) -> $B`

**What's happening:**
- Type variables aren't being properly connected
- `f`'s domain and `x`'s type should unify but they're independent
- Getting `(($A -> $B), $C) -> $D` instead

**Likely causes:**
1. `deferUnifyPoly()` not being called at the right time
2. Type variable copying in `deepCopyTipe()` not preserving relationships
3. Missing unification between function parameters and function calls
4. Order of operations issue in type checking

## Alternative Approach: Constraint Branch

There's an abandoned branch `constraint_type_checking_experiment` that tried a different approach:
- Added one-way constraints instead of bidirectional unification
- Added `constrains []*TipeVar` field to track dependent types
- Goal: preserve polymorphism without over-constraining
- **Status:** Failed completely ("well, it didn't work, but here it is")

The `deepCopyTipe()` approach is more standard and closer to working.

## Testing

```bash
# Build
go build

# Run all tests
go test ./lexer ./parser ./eval ./ast

# Run only type tests
go test ./ast -v

# Test REPL
./crisp

# Test file
./crisp examples/primes.crisp
```

### Test Files
- `ast/tipe_test.go` - Type system tests (7 failing)
- `eval/eval_test.go` - Runtime tests (all passing)
- `examples/primes.crisp` - Example program using type system
- `examples/test.crisp` - Test examples

## Float Support

Floats use separate operators to avoid operator overloading (which conflicts with type inference):

```crisp
# Integers
x = 3 + 4       # 7
y = 2 ^ 3       # 8

# Floats
a = 3.0 +. 4.0  # 7.0
b = 2.0 ^. 3.0  # 8.0

# Conversion
i2f(5)          # 5.0
f2i(3.7)        # 3
```

**Float operators:** `+.`, `-.`, `*.`, `/.`, `%.`, `^.`, `<.`, `<=.`, `>.`, `>=.`

## Native Functions

Built-in functions with explicit type signatures:

```go
// ast/ast.go
type NativeFuncExpr struct {
    Name       string
    DomainTipe Tipe
    RangeTipe  Tipe
    Func       NativeCode
}
```

Currently defined: `i2f: (Int -> Float)`, `f2i: (Float -> Int)`

## Next Steps: Fixing Polymorphic Functions

### Investigation Areas

1. **Check `deferUnifyPoly()` calls** (`ast/tipe.go:298-310`)
   - Is it being called for all function applications?
   - Is it being called too early/late?

2. **Examine `deepCopyTipe()` implementation** (`ast/tipe.go:718-801`)
   - Does it properly track type variable relationships via `varMap`?
   - Are all type variable references being updated?

3. **Trace function parameter binding** (`ast/tipe.go:365-402`)
   - Look at `UserFuncExpr` type inference
   - Check how function domains connect to parameters

4. **Debug with print statements**
   - Add logging to show type variable IDs and unifications
   - Trace `(f,x) -> f(x)` example step by step

5. **Review similar implementations**
   - OCaml's type inference
   - Algorithm W implementations
   - Hindley-Milner papers

### Resources

- **Type inference basics:** Look up "Algorithm W" or "Hindley-Milner"
- **Book reference:** The implementation was inspired by Thorsten Ball's "Writing An Interpreter in Go"
- **Repository author:** Chris Pine (cpine@newrelic.com) - created in July 2020

## Important Notes

- **Priority:** Type inference is MORE important than operator overloading
- **Design choice:** Separate operators for int/float is intentional for type safety
- **Performance:** `eval/eval.go` dispatches on concrete types for speed
- **Lazy evaluation:** Crisp uses lazy semantics (thunks everywhere)
- **Indentation:** Uses TABS, not spaces (semantically significant)

## Previous Attempts

1. **July 17-18, 2020:** Polymorphic type checking implemented
   - Partially working, immediately disabled
   - README updated: "Which makes me sad. Check out my branch where I tried to get type inference working."

2. **July 19, 2020:** Constraint-based approach
   - Complete failure, abandoned same day

3. **July 20, 2020:** Type system deleted for operator overloading
   - ~2000 lines removed
   - Dynamic typing restored

4. **November 2024:** Type system restored with float support
   - This branch brings back the promise of static typing

Good luck! The type system is tantalizingly close to working. 🎯
