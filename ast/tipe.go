package ast

import (
	"crisp/token"
	"fmt"
)

// returns array of error messages
func CheckTipes(exprs []Expr) []string {
	tc := &TipeChecker{}

	// We assign tipe variables recursively to all expressions.

	// `inferTipes()` traverses the AST, assigning tipe variables and gathering
	// additional information about the various tipes. It calls `unify()` which attempts
	// integrates all of that tipe information. Hopefully every expression will have a
	// known tipe by the end.
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
		polyTipe := derefTipeVar(funcApplication.LExpr.TipeVar(tc))
		domainTV := funcApplication.RExpr.TipeVar(tc)
		rangeTV := funcApplication.TipeVar(tc)

		copyTipe := tc.deepCopyTipe(polyTipe.ref).(*FuncTipe)
		tc.unify(copyTipe.Domain, domainTV)
		tc.unify(copyTipe.Range, rangeTV)
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
}

func (tc *TipeChecker) error(err string, a ...interface{}) {
	tc.tcErrors = append(tc.tcErrors, fmt.Sprintf("Crisp type error: "+err+"\n", a...))
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
		tc.unify(tv, boundExpr.TipeVar(tc))

	case *ArgExpr:
		// nothing to do, as the associated FuncExpr handles it

	case *UnopExpr:
		// tipe is the same as the sub-tipe
		tc.unify(tv, expr.Expr.TipeVar(tc))
		// let's see if we can learn more from the token
		if expr.Token.Type == token.Minus {
			// TODO: operator overloading!
			//tc.unify(tv, IntTipe) // could be FloatTipe
		}

		tc.inferTipes(expr.Expr)

	case *BinopExpr:
		ltv := expr.LExpr.TipeVar(tc)
		rtv := expr.RExpr.TipeVar(tc)

		switch expr.Token.Type {
		case token.At:
			fTipe := &FuncTipe{
				Domain: tc.newTipeVar(), // was rtv
				Range:  tc.newTipeVar(), // was tv
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
			tc.unify(tv, BoolTipe)
			tc.unify(ltv, IntTipe)
			tc.unify(rtv, IntTipe)
		case token.FLT, token.FLTE, token.FGT, token.FGTE:
			tc.unify(tv, BoolTipe)
			tc.unify(ltv, FloatTipe)
			tc.unify(rtv, FloatTipe)
		case token.Plus, token.Minus, token.Div, token.Mod:
			tc.unify(tv, IntTipe)
			tc.unify(ltv, IntTipe)
			tc.unify(rtv, IntTipe)
		case token.FPlus, token.FMinus, token.FMult, token.FDiv, token.FMod, token.FExp:
			tc.unify(tv, FloatTipe)
			tc.unify(ltv, FloatTipe)
			tc.unify(rtv, FloatTipe)
		case token.Exp:
			// TODO: operator overloading!
			tc.unify(tv, IntTipe)
			tc.unify(ltv, IntTipe)
			tc.unify(rtv, IntTipe)
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
			// TODO: operator overloading!
			tc.unify(tv, IntTipe)
			tc.unify(ltv, IntTipe)
			tc.unify(rtv, IntTipe)
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
			tipe := tc.newTipeVar()
			tTipe.TipeVars = append(tTipe.TipeVars, tipe)
			tc.unify(e.TipeVar(tc), tipe)
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
			tipe := tc.newTipeVar()
			rTipe.Fields = append(rTipe.Fields, RecordFieldTipe{Name: field.Name, TipeVar: tipe})
			tc.unify(field.Expr.TipeVar(tc), tipe)
		}
		tc.unify(tv, rTipe)

		for _, field := range expr.Fields {
			tc.inferTipes(field.Expr)
		}

	case *ConsExpr:
		nodeTipe := tc.newTipeVar()
		lTipe := &ListTipe{TipeVar: nodeTipe}
		tc.unify(tv, lTipe)
		if !expr.IsNilList() {
			tc.unify(expr.Head.TipeVar(tc), nodeTipe)
			tc.unify(expr.Tail.TipeVar(tc), tv)

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

	case *AssertListIsConsExpr:
		// Note: this expression asserts that it's a cons cell,
		// but we accept an assertion failure.
		// The tipe is List<a> for some tipe a.
		tc.unify(tv, BoolTipe)
		tc.unify(expr.List.TipeVar(tc), &ListTipe{TipeVar: tc.newTipeVar()})

		tc.inferTipes(expr.List)

	// TODO: consider making this the same expression as AssertListIsConsExpr
	case *AssertListIsNilExpr:
		// Note: this expression asserts that it's [],
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

		for _, b := range expr.Env.Bindings {
			tc.inferTipes(b.Expr)
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
func (tc *TipeChecker) unify(tipe0 Tipe, tipe1 Tipe) {
	if tvv0, ok := tipe0.(*TipeVar); ok {
		tv0 := derefTipeVar(tvv0)

		if tvv1, ok := tipe1.(*TipeVar); ok {
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
			// child points to parent
			child.ref = parent
			return
		}

		// tv0 is a dereferenced TipeVar

		tv0.ref = tc.union(tv0.ref, tipe1)
		return
	} else if _, ok := tipe1.(*TipeVar); ok {
		// tipe0 is not a TipeVar, but tipe1 is, so swap and retry
		tc.unify(tipe1, tipe0)
		return
	}

	// so neither tipe0 nor tipe1 are TipeVars
	unionTipe := tc.union(tipe0, tipe1)

	if unionTipe == Empty {
		// not really sure what to do here, but I'll have a
		// better idea once I see it happen
		panic("TODO")
	}
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

			tupleUnion := &TupleTipe{}

			for i, t := range tt0.TipeVars {
				unionVar := tc.newTipeVar()
				tc.unify(unionVar, t)
				tc.unify(unionVar, tt1.TipeVars[i])
				tupleUnion.TipeVars = append(tupleUnion.TipeVars, unionVar)
			}

			return tupleUnion
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
						// both rt0 and rt1 have identically named fields, so unify
						unionVar := tc.newTipeVar()
						tc.unify(unionVar, rt0.Fields[i0].TipeVar)
						tc.unify(unionVar, rt1.Fields[i1].TipeVar)
						unionField := RecordFieldTipe{
							Name:    rt0.Fields[i0].Name,
							TipeVar: unionVar,
						}
						recordUnion.Fields = append(recordUnion.Fields, unionField)
						i0++
						i1++
					}

					return recordUnion
				}
				// rt0 is partial, rt1 is not, therefor rt0 must be a subset
				recordUnion := &RecordTipe{Partial: true}

				len0 := len(rt0.Fields)
				i := 0
				for _, f := range rt1.Fields {
					if i >= len0 {
						// just take from rt1
						recordUnion.Fields = append(recordUnion.Fields, f)
						continue
					}
					if rt0.Fields[i].Name > f.Name {
						// just take from rt1
						recordUnion.Fields = append(recordUnion.Fields, f)
						continue
					}
					if rt0.Fields[i].Name == f.Name {
						// found a matching field
						unionVar := tc.newTipeVar()
						tc.unify(unionVar, rt0.Fields[i].TipeVar)
						tc.unify(unionVar, f.TipeVar)
						unionField := RecordFieldTipe{
							Name:    f.Name,
							TipeVar: unionVar,
						}
						recordUnion.Fields = append(recordUnion.Fields, unionField)
						i++
						continue
					}
					// if we got here, then rt0.Fields[i].Name < f.Name,
					// which means rt0 has a field rt1 lacks,
					// which means they cannot be unified
					return Empty
				}
				return recordUnion
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
			recordUnion := &RecordTipe{}

			for i, f := range rt0.Fields {
				unionVar := tc.newTipeVar()
				tc.unify(unionVar, f.TipeVar)
				tc.unify(unionVar, rt1.Fields[i].TipeVar)
				unionField := RecordFieldTipe{
					Name:    f.Name,
					TipeVar: unionVar,
				}
				recordUnion.Fields = append(recordUnion.Fields, unionField)
			}

			return recordUnion
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
			unionVar := tc.newTipeVar()
			tc.unify(unionVar, lt0.TipeVar)
			tc.unify(unionVar, lt1.TipeVar)

			return &ListTipe{TipeVar: unionVar}
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
			unionDomain := tc.newTipeVar()
			unionRange := tc.newTipeVar()
			unionFunc := &FuncTipe{
				Domain: unionDomain,
				Range:  unionRange,
			}
			tc.unify(unionDomain, ft0.Domain)
			tc.unify(unionDomain, ft1.Domain)
			tc.unify(unionRange, ft0.Range)
			tc.unify(unionRange, ft1.Range)
			return unionFunc
		}
		// tipe0 is a FuncTipe, but tipe1 isn't
		return Empty
	}
	if _, ok := tipe1.(*FuncTipe); ok {
		// tipe1 is a FuncTipe, but tipe0 isn't
		return Empty
	}

	panic("Unhandled type in tc.union()")
	return Empty
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

	case *AssertListIsConsExpr:
		tc.finalizeTipes(expr.List)

	case *AssertListIsNilExpr:
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

	case *SimpleTipe, *OmegaTipe, *EmptyTipe:
		// nothing to do

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

	case *SimpleTipe, *OmegaTipe, *EmptyTipe:
		// nothing to do
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
