package ast

import (
	"crisp/token"
	"fmt"
)

// returns array of error messages
func CheckTipes(exprs []Expr) []string {
	tc := &TipeChecker{
		typeSchemes: make(map[*TipeVar]*TypeScheme),
	}

	// We assign tipe variables recursively to all expressions.

	// `inferTipes()` traverses the AST, assigning tipe variables and gathering
	// additional information about the various tipes. It calls `unify()` which attempts
	// integrates all of that tipe information. Hopefully every expression will have a
	// known tipe by the end.
	// NOTE: Generalization happens DURING inference in the LetExpr case
	for _, expr := range exprs {
		tc.inferTipes(expr)
	}

	// Now we try to handle polymorphic functions. Every function call expression
	// has a type, and a function expression in that call might have a different
	// type than the function on its own. For example, the type of `len` is ([$A…] -> Int)
	// but in a particular call, it might have type ([$Int…] -> Int) or ([$Bool…] -> Int).
	// So what we do here is copy the function type with fresh type variables and
	// unify it with the types we know to be the domain and range for this call.
	for _, funcApplication := range tc.funcApplications {
		// First check if the function has been generalized (has a TypeScheme)
		// If so, we should have already handled it via instantiation in LookupExpr
		// Skip further processing to avoid breaking the polymorphism
		funcExpr := funcApplication.LExpr
		funcTV := derefTipeVar(funcExpr.TipeVar(tc))
		if _, hasScheme := tc.typeSchemes[funcTV]; hasScheme {
			// This function was generalized, instantiation already happened at lookup
			continue
		}

		polyTipe := derefTipeVar(funcExpr.TipeVar(tc))
		domainTV := funcApplication.RExpr.TipeVar(tc)
		rangeTV := funcApplication.TipeVar(tc)

		// If the function type contains Omega (undetermined), unify directly
		// This handles function parameters that aren't yet fully constrained
		// Otherwise, copy the type for polymorphic instantiation
		if funcTipe, ok := polyTipe.ref.(*FuncTipe); ok {
			if tc.hasOmega(funcTipe.Domain) || tc.hasOmega(funcTipe.Range) {
				// Function type is not fully determined, unify directly
				tc.unify(funcTipe.Domain, domainTV)
				tc.unify(funcTipe.Range, rangeTV)
				continue
			}
		}

		// Function type is fully determined (polymorphic function case)
		// Copy it to create a fresh instance for this call site
		copyTipe := tc.deepCopyTipe(polyTipe.ref).(*FuncTipe)
		tc.unify(copyTipe.Domain, domainTV)
		tc.unify(copyTipe.Range, rangeTV)
	}

	// Default numeric types: if a type variable is still NumericTipe (Int|Float) with no
	// other constraints, default it to Int. This is similar to Haskell's defaulting rules.
	for _, expr := range exprs {
		tc.defaultNumericTypes(expr)
	}

	// Now that we have all of the type information, we remove the levels of indirection
	// we racked up (all the type vars pointing to type vars) and record the final types
	// in each expression.
	for _, expr := range exprs {
		tc.finalizeTipes(expr)
	}

	// None of these had better be Empty, because that would mean we failed to find a type.
	for _, expr := range exprs {
		if expr.FinalTipe() == Empty {
			tc.error("type checking failed: resolved to empty type")
		}
	}

	return tc.tcErrors
}

// Store function call expressions so we can copy them later when we
// infer the types of polymorphic functions.
func (tc *TipeChecker) deferUnifyPoly(funcApplication *BinopExpr) {
	tc.funcApplications = append(tc.funcApplications, funcApplication)
}

type TipeChecker struct {
	tcErrors         []string
	funcApplications []*BinopExpr
	typeSchemes      map[*TipeVar]*TypeScheme // Maps type variables to their type schemes
}

func (tc *TipeChecker) error(err string, a ...interface{}) {
	tc.tcErrors = append(tc.tcErrors, fmt.Sprintf("Crisp type error: "+err+"\n", a...))
}

// TypeScheme represents a polymorphic type (∀a.τ)
// It tracks which type variables should be instantiated with fresh variables
type TypeScheme struct {
	BoundVars []*TipeVar // Type variables that are universally quantified
	Type      *TipeVar   // The type expression
}

// the Tipe interface

type Tipe interface {
	// TODO: If we have a type expression with more than 26 type variables,
	// we will run out of letters. Need to fix this.
	TipeString(r rune) (string, rune)
}

// the actual Tipes

type OmegaTipe struct { // the tipe of all values (or zero information)
	String string
}

func (t *OmegaTipe) TipeString(r rune) (string, rune) {
	return t.String, r
}

var Omega = &OmegaTipe{String: "Ω"}

type EmptyTipe struct { // the tipe of no values (failed tipe match)
	String string
}

func (t *EmptyTipe) TipeString(r rune) (string, rune) {
	return t.String, r
}

var Empty = &EmptyTipe{String: "∅"}

type SimpleTipe struct {
	Name string
}

func (t *SimpleTipe) TipeString(r rune) (string, rune) {
	return t.Name, r
}

var UnitTipe = &SimpleTipe{"Unit"}   // the tipe of the zero-tuple: ()
var IntTipe = &SimpleTipe{"Int"}     // the tipe of ints
var FloatTipe = &SimpleTipe{"Float"} // the tipe of floats
var BoolTipe = &SimpleTipe{"Bool"}   // the tipe of bools

// UnionTipe represents a union of types, used for operator overloading
// For example, + can work on either Int or Float
type UnionTipe struct {
	Tipes []Tipe
}

func (t *UnionTipe) TipeString(r rune) (string, rune) {
	if len(t.Tipes) == 0 {
		return "Empty", r
	}
	str := ""
	for i, tipe := range t.Tipes {
		if i > 0 {
			str += "|"
		}
		tipeStr, newR := tipe.TipeString(r)
		str += tipeStr
		r = newR
	}
	return str, r
}

// Helper to create a numeric union (Int | Float)
var NumericTipe = &UnionTipe{[]Tipe{IntTipe, FloatTipe}}

type TupleTipe struct { // the tipe of n-tuples for n >= 2 (there is no 1-tuple)
	TipeVars []*TipeVar
}

func (t *TupleTipe) TipeString(r rune) (string, rune) {
	str := ""

	for i, elem := range t.TipeVars {
		if i < 1 {
			str += "("
		} else {
			str += ", "
		}
		retStr, retRune := elem.TipeString(r)
		str += retStr
		r = retRune
	}
	str += ")"

	return str, r
}

type RecordFieldTipe struct {
	Name    string
	TipeVar *TipeVar
}
type RecordTipe struct {
	Fields []RecordFieldTipe
	// l-values might have partial types, as will record lookups (which only know about one field)
	Partial bool
}

func (t *RecordTipe) TipeString(r rune) (string, rune) {
	str := ""

	for i, field := range t.Fields {
		if i < 1 {
			str += "{"
		} else {
			str += ", "
		}
		str += field.Name + ": "
		retStr, retRune := field.TipeVar.TipeString(r)
		str += retStr
		r = retRune
	}
	str += "}"

	return str, r
}

type ListTipe struct {
	TipeVar *TipeVar // Lists are so nice to tipe check <3
}

func (t *ListTipe) TipeString(r rune) (string, rune) {
	retStr, retRune := t.TipeVar.TipeString(r)
	r = retRune

	return "[" + retStr + "…]", r
}

type FuncTipe struct {
	Domain *TipeVar
	Range  *TipeVar
}

func (t *FuncTipe) TipeString(r rune) (string, rune) {
	dStr, retRuneD := t.Domain.TipeString(r)
	r = retRuneD
	rStr, retRuneR := t.Range.TipeString(r)
	r = retRuneR

	return "(" + dStr + " -> " + rStr + ")", r
}

// Note: We use the ID to impose an ordering on TipeVars,
// so we can avoid cycles in the tipe graph.
type TipeVar struct {
	ID  int
	ref Tipe
	r   rune
}

func (t *TipeVar) TipeString(r rune) (string, rune) {
	if t.ref == Omega {
		if t.r > 0 {
			return "$" + string(t.r), r
		}
		t.r = r
		return "$" + string(t.r), r + 1
	}
	if _, ok := t.ref.(*TipeVar); ok {
		retStr, retRune := t.ref.TipeString(r)
		r = retRune
		return "*" + retStr, r
	}
	retStr, retRune := t.ref.TipeString(r)
	r = retRune

	return retStr, r
}

var numTipeVars = 0

func (tc *TipeChecker) newTipeVar() *TipeVar {
	tv := &TipeVar{ID: numTipeVars, ref: Omega}
	numTipeVars++

	return tv
}

// Takes what might be a chain of TipeVars and returns the last one.
func derefTipeVar(tv *TipeVar) *TipeVar {
	if tv2, ok := tv.ref.(*TipeVar); ok {
		return derefTipeVar(tv2)
	}
	return tv
}

// Helper function to check if a type contains Omega (undetermined type)
func (tc *TipeChecker) hasOmega(someTipe Tipe) bool {
	switch tipe := someTipe.(type) {
	case *OmegaTipe:
		return true
	case *EmptyTipe, *SimpleTipe:
		return false
	case *TupleTipe:
		for _, tv := range tipe.TipeVars {
			if tc.hasOmega(derefTipeVar(tv).ref) {
				return true
			}
		}
		return false
	case *ListTipe:
		return tc.hasOmega(derefTipeVar(tipe.TipeVar).ref)
	case *RecordTipe:
		for _, f := range tipe.Fields {
			if tc.hasOmega(derefTipeVar(f.TipeVar).ref) {
				return true
			}
		}
		return false
	case *FuncTipe:
		return tc.hasOmega(derefTipeVar(tipe.Domain).ref) || tc.hasOmega(derefTipeVar(tipe.Range).ref)
	case *UnionTipe:
		// A union has Omega if any of its members has Omega
		for _, t := range tipe.Tipes {
			if tc.hasOmega(t) {
				return true
			}
		}
		return false
	case *TipeVar:
		return tc.hasOmega(derefTipeVar(tipe).ref)
	default:
		return false
	}
}

// Generalize creates a type scheme from a type by finding all free type variables
// A type variable is "free" if it's still Omega or contains Omega
func (tc *TipeChecker) generalize(tv *TipeVar) *TypeScheme {
	freeVars := make(map[int]*TipeVar)
	tc.collectFreeVars(tv, freeVars)

	boundVars := make([]*TipeVar, 0, len(freeVars))
	for _, v := range freeVars {
		boundVars = append(boundVars, v)
	}

	return &TypeScheme{
		BoundVars: boundVars,
		Type:      tv,
	}
}

// Collect all type variables that contain Omega (are not fully determined)
func (tc *TipeChecker) collectFreeVars(tv *TipeVar, freeVars map[int]*TipeVar) {
	tv = derefTipeVar(tv)

	// If we've already seen this type variable, skip it
	if _, seen := freeVars[tv.ID]; seen {
		return
	}

	switch tipe := tv.ref.(type) {
	case *OmegaTipe:
		// This is a free type variable
		freeVars[tv.ID] = tv

	case *EmptyTipe, *SimpleTipe, *UnionTipe:
		// Fully determined, no free variables
		// (UnionTipe contains concrete types, not type variables)

	case *TupleTipe:
		for _, elemTV := range tipe.TipeVars {
			tc.collectFreeVars(elemTV, freeVars)
		}

	case *ListTipe:
		tc.collectFreeVars(tipe.TipeVar, freeVars)

	case *RecordTipe:
		for _, field := range tipe.Fields {
			tc.collectFreeVars(field.TipeVar, freeVars)
		}

	case *FuncTipe:
		tc.collectFreeVars(tipe.Domain, freeVars)
		tc.collectFreeVars(tipe.Range, freeVars)
	}
}

// Instantiate creates a fresh instance of a type scheme by replacing bound variables
func (tc *TipeChecker) instantiate(scheme *TypeScheme) *TipeVar {
	if len(scheme.BoundVars) == 0 {
		// No bound variables, return the type as-is
		return scheme.Type
	}

	// Create fresh type variables for each bound variable
	substitution := make(map[int]*TipeVar)
	for _, boundVar := range scheme.BoundVars {
		substitution[boundVar.ID] = tc.newTipeVar()
	}

	// Apply the substitution to create a fresh copy
	return tc.applySubstitution(scheme.Type, substitution)
}

// Apply a substitution to a type variable, creating a fresh copy
func (tc *TipeChecker) applySubstitution(tv *TipeVar, substitution map[int]*TipeVar) *TipeVar {
	tv = derefTipeVar(tv)

	// If this type variable should be substituted, return the fresh variable
	if freshTV, found := substitution[tv.ID]; found {
		return freshTV
	}

	// Otherwise, create a new type variable and recursively substitute its contents
	newTV := tc.newTipeVar()

	switch tipe := tv.ref.(type) {
	case *OmegaTipe, *EmptyTipe, *SimpleTipe, *UnionTipe:
		// These types don't contain type variables, so just use as-is
		newTV.ref = tipe

	case *TupleTipe:
		newTuple := &TupleTipe{}
		for _, elemTV := range tipe.TipeVars {
			newTuple.TipeVars = append(newTuple.TipeVars, tc.applySubstitution(elemTV, substitution))
		}
		newTV.ref = newTuple

	case *ListTipe:
		newTV.ref = &ListTipe{
			TipeVar: tc.applySubstitution(tipe.TipeVar, substitution),
		}

	case *RecordTipe:
		newRecord := &RecordTipe{Partial: tipe.Partial}
		for _, field := range tipe.Fields {
			newRecord.Fields = append(newRecord.Fields, RecordFieldTipe{
				Name:    field.Name,
				TipeVar: tc.applySubstitution(field.TipeVar, substitution),
			})
		}
		newTV.ref = newRecord

	case *FuncTipe:
		newTV.ref = &FuncTipe{
			Domain: tc.applySubstitution(tipe.Domain, substitution),
			Range:  tc.applySubstitution(tipe.Range, substitution),
		}
	}

	return newTV
}

// Default numeric types: if a type is still a NumericTipe (Int|Float union),
// default it to Int. This provides backward compatibility with existing tests
// and follows Haskell's defaulting rules for numeric types.
func (tc *TipeChecker) defaultNumericTypes(someExpr Expr) {
	switch expr := someExpr.(type) {
	case *IntExpr, *FloatExpr, *BoolExpr, *UnitExpr:
		// Literals already have concrete types
		tc.defaultNumericType(expr.TipeVar(tc))

	case *LookupExpr:
		tc.defaultNumericType(expr.TipeVar(tc))

	case *ArgExpr:
		tc.defaultNumericType(expr.TipeVar(tc))

	case *UnopExpr:
		tc.defaultNumericType(expr.TipeVar(tc))
		tc.defaultNumericTypes(expr.Expr)

	case *BinopExpr:
		tc.defaultNumericType(expr.TipeVar(tc))
		tc.defaultNumericTypes(expr.LExpr)
		tc.defaultNumericTypes(expr.RExpr)

	case *TupleExpr:
		tc.defaultNumericType(expr.TipeVar(tc))
		for _, e := range expr.Exprs {
			tc.defaultNumericTypes(e)
		}

	case *RecordExpr:
		tc.defaultNumericType(expr.TipeVar(tc))
		for _, f := range expr.Fields {
			tc.defaultNumericTypes(f.Expr)
		}

	case *ConsExpr:
		tc.defaultNumericType(expr.TipeVar(tc))
		if !expr.IsNilList() {
			tc.defaultNumericTypes(expr.Head)
			tc.defaultNumericTypes(expr.Tail)
		}

	case *UserFuncExpr:
		tc.defaultNumericType(expr.TipeVar(tc))
		for _, fp := range expr.FuncPieceExprs {
			tc.defaultNumericTypes(fp)
		}

	case *NativeFuncExpr:
		tc.defaultNumericType(expr.TipeVar(tc))

	case *AssertAnyOfTheseSets:
		for _, set := range expr.AssertSets {
			for _, assert := range set {
				tc.defaultNumericTypes(assert)
			}
		}

	case *TupleDestructureExpr:
		tc.defaultNumericTypes(expr.Tuple)

	case *ConsDestructureExpr:
		tc.defaultNumericTypes(expr.List)

	case *RecordLookupExpr:
		tc.defaultNumericType(expr.TipeVar(tc))
		tc.defaultNumericTypes(expr.Record)

	case *LetExpr:
		tc.defaultNumericType(expr.TipeVar(tc))
		for _, a := range expr.Asserts {
			tc.defaultNumericTypes(a)
		}
		for _, b := range expr.Env.Bindings {
			tc.defaultNumericTypes(b.Expr)
		}
		tc.defaultNumericTypes(expr.Expr)
	}
}

// Default a single type variable from NumericTipe to IntTipe
func (tc *TipeChecker) defaultNumericType(tv *TipeVar) {
	tv = derefTipeVar(tv)
	if tv.ref == NumericTipe {
		tv.ref = IntTipe
	}
	// Recursively default nested types
	switch tipe := tv.ref.(type) {
	case *TupleTipe:
		for _, elemTV := range tipe.TipeVars {
			tc.defaultNumericType(elemTV)
		}
	case *ListTipe:
		tc.defaultNumericType(tipe.TipeVar)
	case *RecordTipe:
		for _, field := range tipe.Fields {
			tc.defaultNumericType(field.TipeVar)
		}
	case *FuncTipe:
		tc.defaultNumericType(tipe.Domain)
		tc.defaultNumericType(tipe.Range)
	}
}

// Check if an expression is a syntactic value (safe to generalize)
// This implements the "value restriction" from ML
func (tc *TipeChecker) isSyntacticValue(expr Expr) bool {
	switch expr.(type) {
	case *UserFuncExpr, *NativeFuncExpr:
		// Only generalize functions, not other values
		// This allows functions to be polymorphic while keeping literals
		// and constructors monomorphic (constrained by their uses)
		return true
	default:
		return false
	}
}

// Traverse the AST and generalize top-level let bindings
func (tc *TipeChecker) generalizeLetBindings(someExpr Expr) {
	switch expr := someExpr.(type) {
	case *LetExpr:
		// Check if this is a function piece (first binding is 'arg')
		isFunctionPiece := len(expr.Env.Bindings) > 0 && expr.Env.Bindings[0].Name == ArgName

		if !isFunctionPiece {
			// Generalize top-level bindings
			for _, b := range expr.Env.Bindings {
				exprTV := b.Expr.TipeVar(tc)
				scheme := tc.generalize(exprTV)
				tc.typeSchemes[exprTV] = scheme
			}
		}

		// Recursively generalize nested expressions
		for _, b := range expr.Env.Bindings {
			tc.generalizeLetBindings(b.Expr)
		}
		tc.generalizeLetBindings(expr.Expr)

	case *UserFuncExpr:
		for _, fp := range expr.FuncPieceExprs {
			tc.generalizeLetBindings(fp)
		}

	case *BinopExpr:
		tc.generalizeLetBindings(expr.LExpr)
		tc.generalizeLetBindings(expr.RExpr)

	case *UnopExpr:
		tc.generalizeLetBindings(expr.Expr)

	case *TupleExpr:
		for _, e := range expr.Exprs {
			tc.generalizeLetBindings(e)
		}

	case *RecordExpr:
		for _, field := range expr.Fields {
			tc.generalizeLetBindings(field.Expr)
		}

	case *ConsExpr:
		if !expr.IsNilList() {
			tc.generalizeLetBindings(expr.Head)
			tc.generalizeLetBindings(expr.Tail)
		}

	case *RecordLookupExpr:
		tc.generalizeLetBindings(expr.Record)

	case *AssertAnyOfTheseSets:
		for _, set := range expr.AssertSets {
			for _, assert := range set {
				tc.generalizeLetBindings(assert)
			}
		}

	// Base cases - no nested expressions to traverse
	case *IntExpr, *FloatExpr, *BoolExpr, *UnitExpr, *LookupExpr, *ArgExpr, *NativeFuncExpr:
		// Nothing to do

	default:
		// For any other expression types (destructuring patterns, etc.), just skip
		// This is safer than panicking during generalization
	}
}

// Type Checking

func (tc *TipeChecker) inferTipes(someExpr Expr) {
	defer func() {
		if r := recover(); r != nil {
			tc.error(r.(error).Error())
		}
	}()

	tv := someExpr.TipeVar(tc)

	switch expr := someExpr.(type) {

	case *UnitExpr:
		// we know this tipe
		tc.unify(tv, UnitTipe)

	case *IntExpr:
		// we know this tipe
		tc.unify(tv, IntTipe)

	case *FloatExpr:
		// we know this tipe
		tc.unify(tv, FloatTipe)

	case *BoolExpr:
		// we know this tipe
		tc.unify(tv, BoolTipe)

	case *LookupExpr:
		boundExpr := expr.Env.Get(expr.Depth, expr.Index)
		boundTV := boundExpr.TipeVar(tc)

		// Check if this type variable has a type scheme (polymorphic)
		// NOTE: We check with the non-dereferenced type variable because that's
		// how we store it in the type schemes map during generalization
		if scheme, hasScheme := tc.typeSchemes[boundTV]; hasScheme {
			// Instantiate with fresh type variables
			freshTV := tc.instantiate(scheme)
			tc.unify(tv, freshTV)
		} else {
			// Not polymorphic, use the type variable directly (dereferenced)
			boundTVDeref := derefTipeVar(boundTV)
			tc.unify(tv, boundTVDeref)
		}

	case *ArgExpr:
		// nothing to do, as the associated FuncExpr handles it

	case *UnopExpr:
		// tipe is the same as the sub-tipe
		tc.unify(tv, expr.Expr.TipeVar(tc))
		// let's see if we can learn more from the token
		if expr.Token.Type == token.Minus {
			// Unary minus now works for both Int and Float
			tc.unify(tv, NumericTipe)
		}

		tc.inferTipes(expr.Expr)

	case *BinopExpr:
		ltv := expr.LExpr.TipeVar(tc)
		rtv := expr.RExpr.TipeVar(tc)

		switch expr.Token.Type {
		case token.At:
			fTipe := &FuncTipe{
				Domain: rtv,
				Range:  tv,
			}
			tc.unify(ltv, fTipe)
			// after tipes are otherwise checked, ensure this function
			// can take an `rtv` and would return a `tv` in that case
			tc.deferUnifyPoly(expr)
		case token.Equal:
			tc.unify(tv, BoolTipe)
			tc.unify(ltv, rtv)
		case token.And, token.Or:
			tc.unify(tv, BoolTipe)
			tc.unify(ltv, BoolTipe)
			tc.unify(rtv, BoolTipe)
		case token.LT, token.LTE, token.GT, token.GTE:
			// Comparison operators now work for both Int and Float
			tc.unify(tv, BoolTipe)
			tc.unify(ltv, NumericTipe)
			tc.unify(rtv, NumericTipe)
			tc.unify(ltv, rtv) // Both sides must be the same numeric type
		case token.FLT, token.FLTE, token.FGT, token.FGTE:
			// Keep explicit float comparisons for backward compatibility
			tc.unify(tv, BoolTipe)
			tc.unify(ltv, FloatTipe)
			tc.unify(rtv, FloatTipe)
		case token.Plus, token.Minus, token.Div, token.Mod:
			// Arithmetic operators now work for both Int and Float
			tc.unify(tv, NumericTipe)
			tc.unify(ltv, NumericTipe)
			tc.unify(rtv, NumericTipe)
			tc.unify(tv, ltv)  // Result is same type as operands
			tc.unify(tv, rtv)
		case token.FPlus, token.FMinus, token.FMult, token.FDiv, token.FMod, token.FExp:
			tc.unify(tv, FloatTipe)
			tc.unify(ltv, FloatTipe)
			tc.unify(rtv, FloatTipe)
		case token.Exp:
			// Exponentiation now works for both Int and Float
			tc.unify(tv, NumericTipe)
			tc.unify(ltv, NumericTipe)
			tc.unify(rtv, NumericTipe)
			tc.unify(tv, ltv)  // Result is same type as base
			tc.unify(tv, rtv)  // And same type as exponent
		case token.DblExp:
			argTipe := tc.newTipeVar()
			funcTipe := &FuncTipe{
				Domain: argTipe,
				Range:  argTipe,
			}
			tc.unify(tv, funcTipe)

			tc.unify(tv, ltv)
			tc.unify(rtv, IntTipe)
		case token.Mult:
			// Multiplication now works for both Int and Float
			tc.unify(tv, NumericTipe)
			tc.unify(ltv, NumericTipe)
			tc.unify(rtv, NumericTipe)
			tc.unify(tv, ltv)  // Result is same type as operands
			tc.unify(tv, rtv)
		case token.DblMult:
			x := tc.newTipeVar()
			y := tc.newTipeVar()
			z := tc.newTipeVar()

			tc.unify(tv, &FuncTipe{Domain: x, Range: z})
			tc.unify(ltv, &FuncTipe{Domain: y, Range: z})
			tc.unify(rtv, &FuncTipe{Domain: x, Range: y})
		default:
			tc.error("Whoops, looks like Chris forgot to implement type-checking"+
				" for a binop expression of type %v", expr.Token)
		}

		tc.inferTipes(expr.LExpr)
		tc.inferTipes(expr.RExpr)

	case *UserFuncExpr:
		fDomain := tc.newTipeVar()
		fRange := tc.newTipeVar()
		fTipe := &FuncTipe{
			Domain: fDomain,
			Range:  fRange,
		}
		tc.unify(tv, fTipe)

		for _, fp := range expr.FuncPieceExprs {
			argBinding := fp.Env.Bindings[0]
			if argBinding.Name != ArgName {
				panic("[type error] something ain't right with this here function")
			}
			tc.unify(fDomain, argBinding.Expr.TipeVar(tc))
			tc.unify(fRange, fp.TipeVar(tc))

			tc.inferTipes(fp)
		}

	case *NativeFuncExpr:
		fDomain := tc.newTipeVar()
		fRange := tc.newTipeVar()
		fTipe := &FuncTipe{
			Domain: fDomain,
			Range:  fRange,
		}
		tc.unify(tv, fTipe)
		tc.unify(fDomain, expr.DomainTipe)
		tc.unify(fRange, expr.RangeTipe)

	case *TupleExpr:
		tTipe := &TupleTipe{}

		for _, e := range expr.Exprs {
			tTipe.TipeVars = append(tTipe.TipeVars, e.TipeVar(tc))
		}

		tc.unify(tv, tTipe)

		for _, e := range expr.Exprs {
			tc.inferTipes(e)
		}

	case *RecordExpr:
		rTipe := &RecordTipe{
			Partial: expr.Partial,
		}
		for _, field := range expr.Fields {
			rTipe.Fields = append(rTipe.Fields, RecordFieldTipe{Name: field.Name, TipeVar: field.Expr.TipeVar(tc)})
		}
		tc.unify(tv, rTipe)

		for _, field := range expr.Fields {
			tc.inferTipes(field.Expr)
		}

	case *ConsExpr:
		if expr.IsNilList() {
			nodeTipe := tc.newTipeVar()
			lTipe := &ListTipe{TipeVar: nodeTipe}
			tc.unify(tv, lTipe)
		} else {
			lTipe := &ListTipe{TipeVar: expr.Head.TipeVar(tc)}
			tc.unify(tv, lTipe)
			tc.unify(tv, expr.Tail.TipeVar(tc))

			tc.inferTipes(expr.Head)
			tc.inferTipes(expr.Tail)
		}

	case *RecordLookupExpr:
		rTipe := &RecordTipe{
			Partial: expr.Partial,
		}
		var ftv *TipeVar
		for _, name := range expr.Names {
			fieldTV := tc.newTipeVar()
			if name == expr.Name {
				ftv = fieldTV
			}
			rTipe.Fields = append(rTipe.Fields, RecordFieldTipe{Name: name, TipeVar: fieldTV})
		}
		tc.unify(tv, ftv)
		tc.unify(expr.Record.TipeVar(tc), rTipe)

		tc.inferTipes(expr.Record)

	case *TupleDestructureExpr:
		tTipe := &TupleTipe{}
		for i := 0; i < expr.Size; i++ {
			tTipe.TipeVars = append(tTipe.TipeVars, tc.newTipeVar())
		}
		tc.unify(tv, tTipe.TipeVars[expr.Index])
		tc.unify(expr.Tuple.TipeVar(tc), tTipe)

		tc.inferTipes(expr.Tuple)

	case *ConsDestructureExpr:
		nodeTipe := tc.newTipeVar()
		lTipe := &ListTipe{TipeVar: nodeTipe}
		tc.unify(expr.List.TipeVar(tc), lTipe)

		if expr.IsHead {
			tc.unify(tv, nodeTipe)
		} else {
			tc.unify(tv, expr.List.TipeVar(tc))
		}

		tc.inferTipes(expr.List)

	case *AssertEqualExpr:
		tc.unify(tv, BoolTipe)
		tc.unify(expr.LExpr.TipeVar(tc), expr.RExpr.TipeVar(tc))

		tc.inferTipes(expr.LExpr)
		tc.inferTipes(expr.RExpr)

	case *AssertListIsConsOrNilExpr:
		// Note: this expression asserts that it's a cons cell,
		// but we accept an assertion failure.
		// The tipe is List<a> for some tipe a.
		tc.unify(tv, BoolTipe)
		tc.unify(expr.List.TipeVar(tc), &ListTipe{TipeVar: tc.newTipeVar()})

		tc.inferTipes(expr.List)

	case *AssertAnyOfTheseSets:
		tc.unify(tv, BoolTipe)
		for _, set := range expr.AssertSets {
			for _, assert := range set {
				tc.inferTipes(assert)
			}
		}

	case *LetExpr:
		for _, a := range expr.Asserts {
			tc.unify(a.TipeVar(tc), BoolTipe)
			tc.inferTipes(a)
		}

		// Infer and generalize bindings one at a time
		// This allows later bindings to use polymorphic versions of earlier bindings
		//
		// Value Restriction: Only generalize syntactic values (functions),
		// not computed values (applications, etc). This prevents over-generalization.
		isFunctionPiece := len(expr.Env.Bindings) > 0 && expr.Env.Bindings[0].Name == ArgName
		for _, b := range expr.Env.Bindings {
			tc.inferTipes(b.Expr)

			// Generalize immediately after inference if it's a syntactic value
			// and not a function parameter
			if !isFunctionPiece && tc.isSyntacticValue(b.Expr) {
				exprTV := b.Expr.TipeVar(tc)
				scheme := tc.generalize(exprTV)
				tc.typeSchemes[exprTV] = scheme
			}
		}

		tc.unify(tv, expr.Expr.TipeVar(tc))
		tc.inferTipes(expr.Expr)

	default:
		panic(fmt.Sprintf("type-checking error: unhandled expression %v of type %T",
			someExpr, someExpr))
	}
}

// Here we declare that two tipes should be the same, which means merging
// what information we have for them. The graph looks like TipeVars from
// every expression pointing to other TipeVars which point to still others,
// many-to-one, until a chain terminates in an actual Tipe. If the two Tipes
// are compatible, we take the union of them to get the resultant Tipe.
func (tc *TipeChecker) unify(tvv0 *TipeVar, tipe1 Tipe) {
	tv0 := derefTipeVar(tvv0)

	tvv1, ok := tipe1.(*TipeVar)
	if !ok {
		tv0.ref = tc.union(tv0.ref, tipe1)
		return
	}

	tv1 := derefTipeVar(tvv1)

	// tv0 and tv1 are dereferenced TipeVars

	// don't bother if they are the same tipe already
	if tv0 == tv1 {
		return
	}

	var parent *TipeVar
	var child *TipeVar

	// always point from higher ID (child) TipeVar to lower ID (parent)
	if tv0.ID < tv1.ID {
		parent = tv0
		child = tv1
	} else {
		parent = tv1
		child = tv0
	}

	// parent holds the union tipe
	parent.ref = tc.union(tv0.ref, tv1.ref)
	// child points to parent, NOT TO THE UNION because we
	// must preserve the topology of parent/child connections
	child.ref = parent
}

// Here we take the union of two Tipes and return it. Fairly straightforward
// except for partial RecordTipes, which are a bit of a pain to merge.
func (tc *TipeChecker) union(tipe0 Tipe, tipe1 Tipe) Tipe {
	if _, ok := tipe0.(*TipeVar); ok {
		panic("no type variables allowed in tc.union()")
	}
	if _, ok := tipe1.(*TipeVar); ok {
		panic("no type variables allowed in tc.union()")
	}

	// don't bother if they are the same tipe already
	if tipe0 == tipe1 {
		return tipe0
	}

	// OmegaTipe and EmptyTipe
	if tipe0 == Omega || tipe1 == Empty {
		return tipe1
	}
	if tipe0 == Empty || tipe1 == Omega {
		return tipe0
	}

	// UnionTipe - handle this BEFORE SimpleTipe to allow Int to unify with Int|Float
	if ut0, ok := tipe0.(*UnionTipe); ok {
		if ut1, ok := tipe1.(*UnionTipe); ok {
			// Both are unions - find intersection of members
			var intersection []Tipe
			for _, t0 := range ut0.Tipes {
				for _, t1 := range ut1.Tipes {
					// Try to unify the types - if they're compatible, add to intersection
					if t0 == t1 {
						intersection = append(intersection, t0)
						break
					}
				}
			}
			if len(intersection) == 0 {
				return Empty
			}
			if len(intersection) == 1 {
				return intersection[0]
			}
			return &UnionTipe{Tipes: intersection}
		}
		// tipe0 is union, tipe1 is concrete - check if tipe1 matches any union member
		for _, t := range ut0.Tipes {
			if t == tipe1 {
				return tipe1
			}
		}
		return Empty
	}
	if ut1, ok := tipe1.(*UnionTipe); ok {
		// tipe1 is union, tipe0 is concrete - check if tipe0 matches any union member
		for _, t := range ut1.Tipes {
			if t == tipe0 {
				return tipe0
			}
		}
		return Empty
	}

	// SimpleTipe: UnitTipe, IntTipe, BoolTipe
	if _, ok := tipe0.(*SimpleTipe); ok {
		// Either tipe1 is not a SimpleTipe, or it is, but not equal to tipe0
		// (because we already checked for equality above).
		// Either way, the union is empty
		return Empty
	}

	// TupleTipe
	if tt0, ok := tipe0.(*TupleTipe); ok {
		if tt1, ok := tipe1.(*TupleTipe); ok {
			if len(tt0.TipeVars) != len(tt1.TipeVars) {
				return Empty
			}

			for i, t := range tt0.TipeVars {
				tc.unify(t, tt1.TipeVars[i])
			}

			return tt0
		}
		// tipe0 is a TupleTipe, but tipe1 isn't
		return Empty
	}
	if _, ok := tipe1.(*TupleTipe); ok {
		// tipe1 is a TupleTipe, but tipe0 isn't
		return Empty
	}

	// RecordTipe
	if rt0, ok := tipe0.(*RecordTipe); ok {
		if rt1, ok := tipe1.(*RecordTipe); ok {
			if rt0.Partial {
				if rt1.Partial {
					// deduce merged partial record tipe
					recordUnion := &RecordTipe{Partial: true}

					var i0, i1 int
					for i0 < len(rt0.Fields) && i1 < len(rt1.Fields) {
						if i0 >= len(rt0.Fields) {
							// just take from rt1
							recordUnion.Fields = append(recordUnion.Fields, rt1.Fields[i1])
							i1++
							continue
						}
						if i1 >= len(rt1.Fields) {
							// just take from rt0
							recordUnion.Fields = append(recordUnion.Fields, rt0.Fields[i0])
							i0++
							continue
						}
						if rt0.Fields[i0].Name < rt1.Fields[i1].Name {
							// just take from rt0
							recordUnion.Fields = append(recordUnion.Fields, rt0.Fields[i0])
							i0++
							continue
						}
						if rt0.Fields[i0].Name > rt1.Fields[i1].Name {
							// just take from rt1
							recordUnion.Fields = append(recordUnion.Fields, rt1.Fields[i1])
							i1++
							continue
						}
						// both rt0 and rt1 have an identically named field, so unify
						tc.unify(rt0.Fields[i0].TipeVar, rt1.Fields[i1].TipeVar)
						recordUnion.Fields = append(recordUnion.Fields, rt0.Fields[i0])
						i0++
						i1++
					}

					return recordUnion
				}
				// rt0 is partial, rt1 is not, therefor rt0 must be a subset

				len0 := len(rt0.Fields)
				i := 0
				for _, f := range rt1.Fields {
					if i >= len0 {
						// nothing more to unify, so break
						break
					}
					if rt0.Fields[i].Name > f.Name {
						// this field isn't in rt0 (it's a partial, after all)
						continue
					}
					if rt0.Fields[i].Name == f.Name {
						// found a matching field
						tc.unify(f.TipeVar, rt0.Fields[i].TipeVar)
						i++
						continue
					}
					// if we got here, then rt0.Fields[i].Name < f.Name,
					// which means rt0 has a field rt1 lacks,
					// which means they cannot be unified
					return Empty
				}
				return rt1
			}
			if rt1.Partial {
				// rt1 is partial, while rt0 is not, so swap them and
				// try again (to hit the code path above)
				return tc.union(rt1, rt0)
			}
			// so neither rt0 nor rt1 are partial, which makes this easy
			if len(rt0.Fields) != len(rt1.Fields) {
				return Empty
			}
			for i, f := range rt0.Fields {
				if f.Name != rt1.Fields[i].Name {
					return Empty
				}
			}
			// ok, they have the same field names
			for i, f := range rt0.Fields {
				tc.unify(f.TipeVar, rt1.Fields[i].TipeVar)
			}

			return rt0
		}
		// tipe0 is a RecordTipe, but tipe1 isn't
		return Empty
	}
	if _, ok := tipe1.(*RecordTipe); ok {
		// tipe1 is a RecordTipe, but tipe0 isn't
		return Empty
	}

	// ListTipe
	if lt0, ok := tipe0.(*ListTipe); ok {
		if lt1, ok := tipe1.(*ListTipe); ok {
			tc.unify(lt0.TipeVar, lt1.TipeVar)

			return lt0
		}
		// tipe0 is a ListTipe, but tipe1 isn't
		return Empty
	}
	if _, ok := tipe1.(*ListTipe); ok {
		// tipe1 is a ListTipe, but tipe0 isn't
		return Empty
	}

	// FuncTipe
	if ft0, ok := tipe0.(*FuncTipe); ok {
		if ft1, ok := tipe1.(*FuncTipe); ok {
			tc.unify(ft0.Domain, ft1.Domain)
			tc.unify(ft0.Range, ft1.Range)
			return ft0
		}
		// tipe0 is a FuncTipe, but tipe1 isn't
		return Empty
	}
	if _, ok := tipe1.(*FuncTipe); ok {
		// tipe1 is a FuncTipe, but tipe0 isn't
		return Empty
	}

	panic("Unhandled type in tc.union()")
}

/*
 *  TipeVar Finalization
 */

// Finalize the tipes of this and all sub-expressions.
func (tc *TipeChecker) finalizeTipes(someExpr Expr) {
	tc.setFinalTipe(someExpr)

	switch expr := someExpr.(type) {

	case *UnitExpr, *IntExpr, *FloatExpr, *BoolExpr, *LookupExpr, *ArgExpr, *NativeFuncExpr:
		// nothing more to do

	case *LetExpr:
		for _, a := range expr.Asserts {
			tc.finalizeTipes(a)
		}

		for _, b := range expr.Env.Bindings {
			tc.finalizeTipes(b.Expr)
		}

		tc.finalizeTipes(expr.Expr)

	case *UnopExpr:
		tc.finalizeTipes(expr.Expr)

	case *BinopExpr:
		tc.finalizeTipes(expr.LExpr)
		tc.finalizeTipes(expr.RExpr)

	case *TupleExpr:
		for _, e := range expr.Exprs {
			tc.finalizeTipes(e)
		}

	case *RecordExpr:
		for _, f := range expr.Fields {
			tc.finalizeTipes(f.Expr)
		}

	case *ConsExpr:
		if !expr.IsNilList() {
			tc.finalizeTipes(expr.Head)
			tc.finalizeTipes(expr.Tail)
		}

	case *UserFuncExpr:
		for _, fp := range expr.FuncPieceExprs {
			tc.finalizeTipes(fp)
		}

	case *AssertEqualExpr:
		tc.finalizeTipes(expr.LExpr)
		tc.finalizeTipes(expr.RExpr)

	case *AssertListIsConsOrNilExpr:
		tc.finalizeTipes(expr.List)

	case *AssertAnyOfTheseSets:
		for _, set := range expr.AssertSets {
			for _, assert := range set {
				tc.finalizeTipes(assert)
			}
		}

	case *TupleDestructureExpr:
		tc.finalizeTipes(expr.Tuple)

	case *ConsDestructureExpr:
		tc.finalizeTipes(expr.List)

	case *RecordLookupExpr:
		tc.finalizeTipes(expr.Record)

	default:
		tc.error("failed to finalize type for expression: %v", expr)
	}
}

func (tc *TipeChecker) setFinalTipe(expr Expr) {
	tv := derefTipeVar(expr.TipeVar(tc))
	tc.derefAllTipeVars(tv.ref)
	expr.SetFinalTipe(tv.ref)
}

// Reduce chains of TipeVars to a single TipeVar.
func (tc *TipeChecker) derefAllTipeVars(someTipe Tipe) {
	switch tipe := someTipe.(type) {

	case *SimpleTipe, *OmegaTipe, *EmptyTipe, *UnionTipe:
		// nothing to do (these don't contain type variables)

	case *TupleTipe:
		for i, tvv := range tipe.TipeVars {
			tv := derefTipeVar(tvv)
			tipe.TipeVars[i] = tv
			tc.derefAllTipeVars(tv.ref)
		}

	case *RecordTipe:
		for _, f := range tipe.Fields {
			tv := derefTipeVar(f.TipeVar)
			f.TipeVar = tv
			tc.derefAllTipeVars(tv.ref)
		}

	case *ListTipe:
		tv := derefTipeVar(tipe.TipeVar)
		tipe.TipeVar = tv
		tc.derefAllTipeVars(tv.ref)

	case *FuncTipe:
		tvd := derefTipeVar(tipe.Domain)
		tipe.Domain = tvd
		tc.derefAllTipeVars(tvd.ref)

		tvr := derefTipeVar(tipe.Range)
		tipe.Range = tvr
		tc.derefAllTipeVars(tvr.ref)

	default:
		tc.error("failed to finalize type: %T", tipe)
	}
}

// Get a copy of this Tipe with all new TipeVars, preserving the
// connection topology of the TipeVars involved.
func (tc *TipeChecker) deepCopyTipe(someTipe Tipe) Tipe {
	return tc.deepCopyTipeRec(someTipe, map[int]*TipeVar{})
}
func (tc *TipeChecker) deepCopyTipeRec(someTipe Tipe, varMap map[int]*TipeVar) Tipe {
	switch tipe := someTipe.(type) {

	case *SimpleTipe, *OmegaTipe, *EmptyTipe, *UnionTipe:
		// These don't contain type variables, so just return as-is
		return tipe

	case *TupleTipe:
		newTipe := &TupleTipe{}
		for _, tvv := range tipe.TipeVars {
			tv := derefTipeVar(tvv)
			if newTV, ok := varMap[tv.ID]; ok {
				// use newTV instead of creating another TipeVar
				newTipe.TipeVars = append(newTipe.TipeVars, newTV)
			} else {
				// create new TipeVar and deepCopy its reference
				newTV = tc.newTipeVar()
				varMap[tv.ID] = newTV
				newTV.ref = tc.deepCopyTipeRec(tv.ref, varMap)
				newTipe.TipeVars = append(newTipe.TipeVars, newTV)
			}
		}
		return newTipe

	case *RecordTipe:
		newTipe := &RecordTipe{}
		for _, f := range tipe.Fields {
			tv := derefTipeVar(f.TipeVar)
			if newTV, ok := varMap[tv.ID]; ok {
				// use newTV instead of creating another TipeVar
				newTipe.Fields = append(newTipe.Fields, RecordFieldTipe{
					Name:    f.Name,
					TipeVar: newTV,
				})
			} else {
				// create new TipeVar and deepCopy its reference
				newTV = tc.newTipeVar()
				varMap[tv.ID] = newTV
				newTV.ref = tc.deepCopyTipeRec(tv.ref, varMap)
				newTipe.Fields = append(newTipe.Fields, RecordFieldTipe{
					Name:    f.Name,
					TipeVar: newTV,
				})
			}
		}
		return newTipe

	case *ListTipe:
		newTipe := &ListTipe{}
		tv := derefTipeVar(tipe.TipeVar)
		if newTV, ok := varMap[tv.ID]; ok {
			// use newTV instead of creating another TipeVar
			newTipe.TipeVar = newTV
		} else {
			// create new TipeVar and deepCopy its reference
			newTV = tc.newTipeVar()
			varMap[tv.ID] = newTV
			newTV.ref = tc.deepCopyTipeRec(tv.ref, varMap)
			newTipe.TipeVar = newTV
		}
		return newTipe

	case *FuncTipe:
		newTipe := &FuncTipe{}

		tvd := derefTipeVar(tipe.Domain)
		if newTV, ok := varMap[tvd.ID]; ok {
			// use newTV instead of creating another TipeVar
			newTipe.Domain = newTV
		} else {
			// create new TipeVar and deepCopy its reference
			newTV = tc.newTipeVar()
			varMap[tvd.ID] = newTV
			newTV.ref = tc.deepCopyTipeRec(tvd.ref, varMap)
			newTipe.Domain = newTV
		}

		tvr := derefTipeVar(tipe.Range)
		if newTV, ok := varMap[tvr.ID]; ok {
			// use newTV instead of creating another TipeVar
			newTipe.Range = newTV
		} else {
			// create new TipeVar and deepCopy its reference
			newTV = tc.newTipeVar()
			varMap[tvr.ID] = newTV
			newTV.ref = tc.deepCopyTipeRec(tvr.ref, varMap)
			newTipe.Range = newTV
		}

		return newTipe

	default:
		tc.error("failed to finalize type: %T", tipe)
		return Empty
	}
}
