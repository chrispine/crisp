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
	// TODO: fix outdated comments! ^^^ talk about `registerConstraint` and one-way flow of type information

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

type TipeChecker struct {
	tcErrors []string
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
	ID         int
	ref        Tipe
	constrains []*TipeVar
	r          rune
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
				Domain: tc.newTipeVar(),
				Range:  tc.newTipeVar(),
			}
			//fInstanceTipe := &FuncTipe{
			//	Domain: rtv,
			//	Range:  tv,
			//}
			// This function might be polymorphic, and we don't
			// want to over-constrain it to this particular instance.
			tc.constrain(fTipe.Domain, rtv)
			tc.constrain(fTipe.Range, tv)
			tc.unify(ltv, fTipe)
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
func (tc *TipeChecker) unify(tvv0 *TipeVar, tipe1 Tipe) {
	// We assume as an invariant that any constraints will be
	// on the top-level (dereferenced) TipeVars. We must maintain
	// this invariant throughout this function, including before/after
	// we call unify(), union(), or constrain().
	tv0 := derefTipeVar(tvv0)

	tvv1, ok := tipe1.(*TipeVar)
	if !ok {
		// so tipe1 IS NOT a TipeVar
		tv0.ref = tc.union(tv0.ref, tipe1)
		// We clear out the list, and repopulate it as needed
		// in tc.constrain().
		constrainedVars := tv0.constrains
		tv0.constrains = nil
		for _, constrained := range constrainedVars {
			tc.constrain(tv0, constrained)
		}
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

	// We clear out both `constrains` lists, and repopulate it as needed
	// in tc.constrain().
	constrainedVars := tv0.constrains
	tv0.constrains = nil
	constrainedVars = append(constrainedVars, tv1.constrains...)
	tv1.constrains = nil

	for _, constrained := range constrainedVars {
		tc.constrain(parent, constrained)
	}
}

// TODO: document this more
// We are NOT saying these are equal, but we ARE constraining `constrained`
// by `constrainer`.
func (tc *TipeChecker) constrain(constrainer *TipeVar, constrained *TipeVar) {
	if _, ok := constrainer.ref.(*TipeVar); ok {
		panic("invariant violation: constrainer should always be top-level type variable")
	}

	tvCr := constrainer
	tvCd := derefTipeVar(constrained)

	// OmegaTipe
	if tvCr.ref == Omega {
		// not much of a constraint, is it? "it's definitely some type"
		tvCr.constrains = append(tvCr.constrains, tvCd)
		return
	}

	// EmptyTipe
	if tvCr.ref == Empty {
		// ouch, this is the worst of all constraints
		tvCd.ref = Empty
		tc.error("TYPE ERROR")
		// no need to keep tracking this constraint, and it's already maximally constrained
		return
	}

	// tvCd.ref == Empty
	if tvCd.ref == Empty {
		// can't get much more constrained than this, so just return
		return
	}

	// SimpleTipe
	if cr, ok := tvCr.ref.(*SimpleTipe); ok {
		if tvCd.ref == Omega {
			tvCd.ref = cr
			// keep constraining
			tvCr.constrains = append(tvCr.constrains, tvCd)
			return
		}
		if cd, ok := tvCd.ref.(*SimpleTipe); ok {
			if cr != cd {
				// fail
				tvCd.ref = Empty
				tc.error("TYPE ERROR")
				// no need to keep tracking this constraint, and it's already maximally constrained
				return
			}
			// keep constraining
			tvCr.constrains = append(tvCr.constrains, tvCd)
			return
		}
	}

	// TupleTipe
	if cr, ok := tvCr.ref.(*TupleTipe); ok {
		if tvCd.ref == Omega {
			newCd := &TupleTipe{}
			for _, newTvvCr := range cr.TipeVars {
				newTvCr := derefTipeVar(newTvvCr)
				newTvCd := tc.newTipeVar()
				newCd.TipeVars = append(newCd.TipeVars, newTvCd)
				tc.constrain(newTvCr, newTvCd)
			}
			tvCd.ref = newCd
			return
		}
		if cd, ok := tvCd.ref.(*TupleTipe); ok {
			for i, newTvvCr := range cr.TipeVars {
				newTvCr := derefTipeVar(newTvvCr)
				newTvCd := derefTipeVar(cd.TipeVars[i])
				tc.constrain(newTvCr, newTvCd)
			}
			return
		}
	}

	// RecordTipe
	if cr, ok := tvCr.ref.(*RecordTipe); ok {
		// TODO
		cr = cr
		return
	}

	// ListTipe
	if cr, ok := tvCr.ref.(*ListTipe); ok {
		newTvCr := derefTipeVar(cr.TipeVar)
		if tvCd.ref == Omega {
			newTvCd := tc.newTipeVar()
			tvCd.ref = &ListTipe{TipeVar: newTvCd}
			tc.constrain(newTvCr, newTvCd)
			return
		}
		if cd, ok := tvCd.ref.(*ListTipe); ok {
			newTvCd := derefTipeVar(cd.TipeVar)
			tc.constrain(newTvCr, newTvCd)
			return
		}
	}

	// FuncTipe
	if cr, ok := tvCr.ref.(*FuncTipe); ok {
		newTvCrDomain := derefTipeVar(cr.Domain)
		newTvCrRange := derefTipeVar(cr.Range)
		if tvCd.ref == Omega {
			newTvCdDomain := tc.newTipeVar()
			newTvCdRange := tc.newTipeVar()
			tvCd.ref = &FuncTipe{Domain: newTvCdDomain, Range: newTvCdRange}
			tc.constrain(newTvCrDomain, newTvCdDomain)
			tc.constrain(newTvCrRange, newTvCdRange)
			return
		}
		if cd, ok := tvCd.ref.(*FuncTipe); ok {
			newTvCdDomain := derefTipeVar(cd.Domain)
			newTvCdRange := derefTipeVar(cd.Range)
			tc.constrain(newTvCrDomain, newTvCdDomain)
			tc.constrain(newTvCrRange, newTvCdRange)
			return
		}
	}

	panic("unhandled constraint")
}

// Here we take the union of two Tipes and return it. Fairly straightforward
// except for partial RecordTipes, which are a bit of a pain to merge.
func (tc *TipeChecker) union(tipe0 Tipe, tipe1 Tipe) Tipe {
	// Invariant: union() never takes TipeVars, and never returns them.
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
		tc.error("TYPE ERROR")
		return Empty
	}

	// TupleTipe
	if tt0, ok := tipe0.(*TupleTipe); ok {
		if tt1, ok := tipe1.(*TupleTipe); ok {
			if len(tt0.TipeVars) != len(tt1.TipeVars) {
				tc.error("TYPE ERROR")
				return Empty
			}

			for i, t := range tt0.TipeVars {
				tc.unify(t, tt1.TipeVars[i])
			}

			return tt0
		}
		// tipe0 is a TupleTipe, but tipe1 isn't
		tc.error("TYPE ERROR")
		return Empty
	}
	if _, ok := tipe1.(*TupleTipe); ok {
		// tipe1 is a TupleTipe, but tipe0 isn't
		tc.error("TYPE ERROR")
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
					tc.error("TYPE ERROR")
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
				tc.error("TYPE ERROR")
				return Empty
			}
			for i, f := range rt0.Fields {
				if f.Name != rt1.Fields[i].Name {
					tc.error("TYPE ERROR")
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
		tc.error("TYPE ERROR")
		return Empty
	}
	if _, ok := tipe1.(*RecordTipe); ok {
		// tipe1 is a RecordTipe, but tipe0 isn't
		tc.error("TYPE ERROR")
		return Empty
	}

	// ListTipe
	if lt0, ok := tipe0.(*ListTipe); ok {
		if lt1, ok := tipe1.(*ListTipe); ok {
			tc.unify(lt0.TipeVar, lt1.TipeVar)

			return lt0
		}
		// tipe0 is a ListTipe, but tipe1 isn't
		tc.error("TYPE ERROR")
		return Empty
	}
	if _, ok := tipe1.(*ListTipe); ok {
		// tipe1 is a ListTipe, but tipe0 isn't
		tc.error("TYPE ERROR")
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
		tc.error("TYPE ERROR")
		return Empty
	}
	if _, ok := tipe1.(*FuncTipe); ok {
		// tipe1 is a FuncTipe, but tipe0 isn't
		tc.error("TYPE ERROR")
		return Empty
	}

	panic("Unhandled type in tc.union()")
	tc.error("TYPE ERROR")
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
