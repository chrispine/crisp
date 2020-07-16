package ast

import (
	"crisp/token"
	"fmt"
	"strconv"
)

// returns array of error messages
func CheckTipes(exprs []Expr) []string {
	tc := &TipeChecker{}

	// We assign tipe variables recursively to all expressions. All tipe variables are
	// tracked in `dict`, initially pointing to `nil`, which means "we have no information
	// yet about this tipe, so it could be anything".

	// `inferTipes()` traverses the AST, assigning the tipe variables and gathering
	// additional information about the various tipes. It calls `unify()` which attempts
	// integrates all of that tipe information. Hopefully every expression will have a
	// known tipe by the end.
	for _, expr := range exprs {
		tc.inferTipes(expr)
	}

	// Now that we have all of this implicit tipe information in `dict`, let's pull
	// out the final tipes and assign them to the expressions themselves.
	for _, expr := range exprs {
		tc.finalizeTipes(expr)
	}

	return tc.tcErrors
}

type TipeChecker struct {
	refs     []*TipeRef
	tcErrors []string
}

func (tc *TipeChecker) error(err string, a ...interface{}) {
	tc.tcErrors = append(tc.tcErrors, fmt.Sprintf("Crisp type error: "+err+"\n", a...))
}

// the Tipe interface

type Tipe interface {
	TipeString() string
}

// the actual Tipes

type OmegaTipe struct {
	String string
}

func (t *OmegaTipe) TipeString() string {
	return t.String
}

var Omega = &OmegaTipe{String: "Ω"}

type SimpleTipe struct {
	Name string
}

func (t *SimpleTipe) TipeString() string {
	return t.Name
}

var UnitTipe = &SimpleTipe{"Unit"}
var IntTipe = &SimpleTipe{"Int"}
var BoolTipe = &SimpleTipe{"Bool"}

type TupleTipe struct {
	Tipes []Tipe
}

func (t *TupleTipe) TipeString() string {
	str := ""

	for i, elem := range t.Tipes {
		if i < 1 {
			str += "("
		} else {
			str += ", "
		}
		str += elem.TipeString()
	}
	str += ")"

	return str
}

type RecordFieldTipe struct {
	Name string
	Tipe Tipe
}
type RecordTipe struct {
	Fields  []RecordFieldTipe
	Partial bool // l-values might have partial types, as will record lookups (which only know about one field)
}

func (t *RecordTipe) TipeString() string {
	str := ""

	for i, field := range t.Fields {
		if i < 1 {
			str += "{"
		} else {
			str += ", "
		}
		str += field.Name + ": " + field.Tipe.TipeString()
	}
	str += "}"

	return str

}

type ListTipe struct {
	Tipe Tipe
}

func (t *ListTipe) TipeString() string {
	if isNil(t.Tipe) {
		return "[]"
	}
	return "[" + t.Tipe.TipeString() + "…]"
}

type FuncTipe struct {
	Domain Tipe
	Range  Tipe
}

func (t *FuncTipe) TipeString() string {
	return "(" + t.Domain.TipeString() + " -> " + t.Range.TipeString() + ")"
}

// in an ambiguous tipe, the assumption is that exactly one of
// tipes is correct, but we don't know which one
type AmbiguousTipe struct {
	Tipes []Tipe
}

func (t *AmbiguousTipe) TipeString() string {
	str := "Amb"

	for i, elem := range t.Tipes {
		if i < 1 {
			str += "«"
		} else {
			str += ", "
		}
		str += elem.TipeString()
	}
	str += "»"

	return str
}

// sort of like tuple destructuring, but for tipes
type TupleIndexTipe struct {
	Index int
	Size  int
	Tuple Tipe
}

func (t *TupleIndexTipe) TipeString() string {
	return t.Tuple.TipeString() + "@" + strconv.Itoa(t.Index)
}

// Note: We use the ID to impose an ordering on TipeRefs,
// so we can avoid cycles in the tipe graph.
type TipeRef struct {
	ID  int
	ref Tipe
}

func (t *TipeRef) TipeString() string { return "*" + t.ref.TipeString() }

var numTipeRefs = 0

func (tc *TipeChecker) newTipeRef() *TipeRef {
	tr := &TipeRef{ID: numTipeRefs, ref: Omega}
	numTipeRefs++

	// make sure to keep track of all tipe variables
	tc.refs = append(tc.refs, tr)

	return tr
}

type TipeVar struct {
	ID int
}

func (t *TipeVar) TipeString() string { return "$" + strconv.Itoa(t.ID) }

var numTipeVars = 0

func (tc *TipeChecker) newTipeVar() *TipeVar {
	tr := &TipeVar{ID: numTipeVars}
	numTipeVars++

	return tr
}

// Type Checking

func (tc *TipeChecker) inferTipes(someExpr Expr) {
	defer func() {
		if r := recover(); r != nil {
			tc.error(r.(error).Error())
		}
	}()

	tr := someExpr.TipeRef(tc)

	switch expr := someExpr.(type) {

	case *UnitExpr:
		// we know this tipe
		tc.unify(tr, UnitTipe)

	case *IntExpr:
		// we know this tipe
		tc.unify(tr, IntTipe)

	case *BoolExpr:
		// we know this tipe
		tc.unify(tr, BoolTipe)

	case *LookupExpr:
		boundExpr := expr.Env.Get(expr.Depth, expr.Index)
		tc.unify(tr, boundExpr.TipeRef(tc))

	case *ArgExpr:
		// nothing to do, as the associated FuncExpr handles it

	case *UnopExpr:
		// tipe is the same as the sub-tipe
		tc.unify(tr, expr.Expr.TipeRef(tc))
		// let's see if we can learn more from the token
		if expr.Token.Type == token.Minus {
			tc.unify(tr, IntTipe)
		}

		tc.inferTipes(expr.Expr)

	case *BinopExpr:
		var tipe Tipe = Omega
		var lTipe Tipe = Omega
		var rTipe Tipe = Omega

		switch expr.Token.Type {
		case token.At:
			lTipe = &FuncTipe{
				Domain: expr.RExpr.TipeRef(tc),
				Range:  tr,
			}
		case token.Equal:
			tipe = BoolTipe
			lTipe = expr.RExpr.TipeRef(tc)
		case token.And, token.Or:
			tipe = BoolTipe
			lTipe = BoolTipe
			rTipe = BoolTipe
		case token.LT, token.LTE, token.GT, token.GTE:
			tipe = BoolTipe
			lTipe = IntTipe
			rTipe = IntTipe
		case token.Plus, token.Minus, token.Div, token.Mod:
			tipe = IntTipe
			lTipe = IntTipe
			rTipe = IntTipe
		case token.Exp:
			argTipe := tc.newTipeRef()
			funcTipe := &FuncTipe{
				Domain: argTipe,
				Range:  argTipe,
			}
			ambiTipe := &AmbiguousTipe{Tipes: []Tipe{IntTipe, funcTipe}}
			tipe = ambiTipe
			lTipe = ambiTipe
			rTipe = IntTipe
		case token.Mult:
			// This is a bit tricky... we have to make a pair of "fake" 3-tuple types,
			// so we can construct an AmbiguousType of them, because we want *either* all three
			// to be integers, *or* all three to be functions (of the appropriate types),
			// for a total of 2 options. (If we made each one an AmbiguousType, that would be
			// 8 options.)
			x := tc.newTipeRef()
			y := tc.newTipeRef()
			z := tc.newTipeRef()

			xy := &FuncTipe{Domain: x, Range: y}
			yz := &FuncTipe{Domain: y, Range: z}
			xz := &FuncTipe{Domain: x, Range: z}

			intTuple := &TupleTipe{[]Tipe{IntTipe, IntTipe, IntTipe}}
			funcTuple := &TupleTipe{[]Tipe{xz, yz, xy}}
			varTuple := &TupleTipe{[]Tipe{tr, expr.LExpr.TipeRef(tc), expr.RExpr.TipeRef(tc)}}

			tc.unify(varTuple, &AmbiguousTipe{[]Tipe{intTuple, funcTuple}})
		default:
			tc.error("Whoops, looks like Chris forgot to implement type-checking for a binop expression of type %v", expr.Token)
		}

		tc.unify(tr, tipe)
		tc.unify(expr.LExpr.TipeRef(tc), lTipe)
		tc.unify(expr.RExpr.TipeRef(tc), rTipe)

		tc.inferTipes(expr.LExpr)
		tc.inferTipes(expr.RExpr)

	case *FuncExpr:
		fDomain := tc.newTipeRef()
		fRange := tc.newTipeRef()
		fTipe := &FuncTipe{
			Domain: fDomain,
			Range:  fRange,
		}
		tc.unify(tr, fTipe)

		for _, fp := range expr.FuncPieceExprs {
			argBinding := fp.Env.Bindings[0]
			if argBinding.Name != ArgName {
				panic("[type error] something ain't right with this here function")
			}
			tc.unify(fDomain, argBinding.Expr.TipeRef(tc))
			tc.unify(fRange, fp.TipeRef(tc))

			tc.inferTipes(fp)
		}

	case *TupleExpr:
		tTipe := &TupleTipe{}

		for _, e := range expr.Exprs {
			tipe := tc.newTipeRef()
			tTipe.Tipes = append(tTipe.Tipes, tipe)
			tc.unify(e.TipeRef(tc), tipe)
		}

		tc.unify(tr, tTipe)

		for _, e := range expr.Exprs {
			tc.inferTipes(e)
		}

	case *RecordExpr:
		rTipe := &RecordTipe{
			Partial: expr.Partial,
		}
		for _, field := range expr.Fields {
			tipe := tc.newTipeRef()
			rTipe.Fields = append(rTipe.Fields, RecordFieldTipe{Name: field.Name, Tipe: tipe})
			tc.unify(field.Expr.TipeRef(tc), tipe)
		}
		tc.unify(tr, rTipe)

		for _, field := range expr.Fields {
			tc.inferTipes(field.Expr)
		}

	case *ConsExpr:
		nodeTipe := tc.newTipeRef()
		lTipe := &ListTipe{Tipe: nodeTipe}
		tc.unify(tr, lTipe)
		if !expr.IsNilList() {
			tc.unify(expr.Head.TipeRef(tc), nodeTipe)
			tc.unify(expr.Tail.TipeRef(tc), lTipe)

			tc.inferTipes(expr.Head)
			tc.inferTipes(expr.Tail)
		}

	case *RecordLookupExpr:
		rTipe := &RecordTipe{
			Partial: expr.Partial,
		}
		var idx int
		for i, name := range expr.Names {
			if name == expr.Name {
				idx = i
			}
			rTipe.Fields = append(rTipe.Fields, RecordFieldTipe{Name: name, Tipe: tc.newTipeRef()})
		}
		tipe := rTipe.Fields[idx].Tipe
		tc.unify(tr, tipe)
		tc.unify(expr.Record.TipeRef(tc), rTipe)

		tc.inferTipes(expr.Record)

	case *TupleDestructureExpr:
		tTipe := &TupleTipe{}
		for i := 0; i < expr.Size; i++ {
			tTipe.Tipes = append(tTipe.Tipes, tc.newTipeRef())
		}
		tc.unify(tr, tTipe.Tipes[expr.Index])
		tc.unify(expr.Tuple.TipeRef(tc), tTipe)

		tc.inferTipes(expr.Tuple)

	case *ConsDestructureExpr:
		nodeTipe := tc.newTipeRef()
		cTipe := &ListTipe{Tipe: nodeTipe}

		if expr.IsHead {
			tc.unify(tr, nodeTipe)
		} else {
			tc.unify(tr, cTipe)
		}
		tc.unify(expr.List.TipeRef(tc), cTipe)

		tc.inferTipes(expr.List)

	case *AssertEqualExpr:
		tc.unify(tr, BoolTipe)
		tipe := tc.newTipeRef()
		tc.unify(expr.LExpr.TipeRef(tc), tipe)
		tc.unify(expr.RExpr.TipeRef(tc), tipe)

		tc.inferTipes(expr.LExpr)
		tc.inferTipes(expr.RExpr)

	case *AssertListIsConsExpr:
		// Note: this expression asserts that it's a cons cell,
		// but we accept an assertion failure.
		// The tipe is List<a> for some tipe a.
		tc.unify(tr, BoolTipe)
		tc.unify(expr.List.TipeRef(tc), &ListTipe{Tipe: tc.newTipeRef()})

		tc.inferTipes(expr.List)

	case *AssertListIsNilExpr:
		// Note: this expression asserts that it's [],
		// but we accept an assertion failure.
		// The tipe is List<a> for some tipe a.
		tc.unify(tr, BoolTipe)
		tc.unify(expr.List.TipeRef(tc), &ListTipe{Tipe: tc.newTipeRef()})

		tc.inferTipes(expr.List)

	case *AssertAnyOfTheseSets:
		tc.unify(tr, BoolTipe)
		for _, set := range expr.AssertSets {
			for _, assert := range set {
				tc.inferTipes(assert)
			}
		}

	case *LetExpr:
		tc.unify(tr, expr.Expr.TipeRef(tc))

		for _, a := range expr.Asserts {
			tc.unify(a.TipeRef(tc), BoolTipe)
			tc.inferTipes(a)
		}

		for _, b := range expr.Env.Bindings {
			tc.inferTipes(b.Expr)
		}

		tc.inferTipes(expr.Expr)

	default:
		panic(fmt.Sprintf("type-checking error: unhandled expression %v of type %T", someExpr, someExpr))
	}
}

func (tc *TipeChecker) unify(tipe0 Tipe, tipe1 Tipe) {
	if tipe0 == Omega || tipe1 == Omega || tipe0 == tipe1 {
		// no new information to assimilate, so just return
		return
	}

	// resolve formerly-ambiguous tipes
	if ambiTipe, ok := tipe0.(*AmbiguousTipe); ok {
		if len(ambiTipe.Tipes) < 2 {
			tc.unify(ambiTipe.Tipes[0], tipe1)
			return
		}
	}
	if ambiTipe, ok := tipe1.(*AmbiguousTipe); ok {
		if len(ambiTipe.Tipes) < 2 {
			tc.unify(tipe0, ambiTipe.Tipes[0])
			return
		}
	}

	if tr0, ok := tipe0.(*TipeRef); ok {
		deref0 := tr0.ref

		if tr1, ok := tipe1.(*TipeRef); ok {
			// we have two TipeRefs
			if tr1.ID > tr0.ID {
				// if TipeRefs point to other TipeRefs, it must always be
				// to one with a lower ID (to avoid cycles)
				tc.unify(tipe1, tipe0)
				return
			}
			// both are TipeRefs, and tr0 is allowed to point to tr1
			if deref0 == Omega {
				// this holds whether or not tr1 points to Omega
				tr0.ref = tr1
				return
			}
			// tr0 points to something
			deref1 := tr1.ref
			if deref1 == Omega {
				tc.unify(tipe1, deref0)
				return
			}
			// tr0 and tr1 both point to something
			tc.unify(deref0, deref1)
			return
		}
		// tr0 is a TipeRef, tipe1 is not
		if deref0 == Omega {
			tr0.ref = tipe1
			return
		}
		// tr0 is a TipeRef pointing to oldTipe, which is not a TipeRef
		// tipe1 is not a TipeRef
		tc.unify(deref0, tipe1)
		return
	}
	// tipe0 is not a TipeRef
	if _, ok := tipe1.(*TipeRef); ok {
		// tipe1 is a TipeRef, so swap them and try again
		tc.unify(tipe1, tipe0)
		return
	}

	// neither tipe0 nor tipe1 are: TipeRef
	if tt0, ok := tipe0.(*TupleTipe); ok {
		if tt1, ok := tipe1.(*TupleTipe); ok {
			if len(tt0.Tipes) != len(tt1.Tipes) {
				tc.error("incompatible tuple types")
				return
			}
			for i, t := range tt0.Tipes {
				tc.unify(t, tt1.Tipes[i])
			}
			return
		}
		if _, ok := tipe1.(*SimpleTipe); ok {
			tc.error("can't use simple type as tuple type")
			return
		}
		for i, t := range tt0.Tipes {
			tc.unify(t, &TupleIndexTipe{
				Index: i,
				Size:  len(tt0.Tipes),
				Tuple: tipe1,
			})
		}
		return
	}
	if _, ok := tipe1.(*TupleTipe); ok {
		// tipe1 is a TupleTipe, so swap them and try again
		tc.unify(tipe1, tipe0)
		return
	}

	// neither tipe0 nor tipe1 are: TipeRef, TupleTipe
	if ti0, ok := tipe0.(*TupleIndexTipe); ok {
		allegedTuple := ti0.Tuple
		if tuple, ok := allegedTuple.(*TupleTipe); ok {
			if len(tuple.Tipes) != ti0.Size {
				tc.error("tuple sizes don't match")
				return
			}
			tc.unify(tuple.Tipes[ti0.Index], tipe1)
			return
		}
		// tipe0 is TupleIndexTipe, but tipe0.Tuple is not a TupleTipe
		if ambiTuple, ok := allegedTuple.(*AmbiguousTipe); ok {
			tc.resolveAmbiTipe(ambiTuple, func(t Tipe) bool {
				if tuple, ok := t.(*TupleTipe); ok {
					// ti0   : our TupleIndexTipe
					// tuple : one possible tuple being indexed
					// tipe1 : the tipe we are unifying with the TupleIndexTipe
					if ti0.Size != len(tuple.Tipes) {
						return false
					}
					m := tc.match(tuple.Tipes[ti0.Index], tipe1)
					return m == Yes || m == Maybe
				}
				// what do we do if it's not a TupleTipe?
				panic("TODO")
				return false // no idea what to return here
			})
			if len(ambiTuple.Tipes) == 1 {
				ti0.Tuple = ambiTuple.Tipes[0]
				tc.unify(tipe0, tipe1)
				return
			}
			// TODO: How do we make sure we don't throw away info?
			// ambiTuple is still ambiguous, but we still need to
			// unify ambiTuple[idx] and tipe1
			return
		}
		// what is ambiTuple then?
		panic("TODO")
		return
	}
	if _, ok := tipe1.(*TupleIndexTipe); ok {
		// tipe1 is a TupleIndexTipe, so swap them and try again
		tc.unify(tipe1, tipe0)
		return
	}

	// neither tipe0 nor tipe1 are: TipeRef, TupleTipe, TupleIndexTipe
	if rt0, ok := tipe0.(*RecordTipe); ok {
		if rt1, ok := tipe1.(*RecordTipe); ok {
			if rt0.Partial {
				if rt1.Partial {
					panic("TODO")
					return
				}
				// rt0 is partial, rt1 is not, therefor rt0 must be a subset
				len0 := len(rt0.Fields)
				i := 0
				for _, f := range rt1.Fields {
					if i >= len0 {
						// no more fields in the partial to unify
						break
					}
					if rt0.Fields[i].Name > f.Name {
						// keep looking for the next field both types contain
						continue
					}
					if rt0.Fields[i].Name == f.Name {
						// found a matching field
						tc.unify(rt0.Fields[i].Tipe, f.Tipe)
						i++
						continue
					}
					// if we got here, then rt0.Fields[i].Name < f.Name,
					// which means rt0 has a field rt1 lacks,
					// which means they cannot be unified
					tc.error("incompatible record types")
					break
				}
				return
			}
			if rt1.Partial {
				// rt1 is partial, while rt0 is not, so swap them
				// and try again (to hit the code path above)
				tc.unify(rt1, rt0)
				return
			}
			if len(rt0.Fields) != len(rt1.Fields) {
				tc.error("incompatible record types")
				return
			}
			for i, f := range rt0.Fields {
				if f.Name != rt1.Fields[i].Name {
					tc.error("incompatible record types")
					return
				}
			}
			// ok, they have the same field names
			for i, f := range rt0.Fields {
				if f.Name != rt1.Fields[i].Name {
					tc.unify(f.Tipe, rt1.Fields[i].Tipe)
				}
			}
			return
		}
		if _, ok := tipe1.(*SimpleTipe); ok {
			tc.error("can't use simple type as record type")
			return
		}
		panic("TODO")
		return
	}
	if _, ok := tipe1.(*RecordTipe); ok {
		// tipe1 is a RecordTipe, so swap them and try again
		tc.unify(tipe1, tipe0)
		return
	}

	// neither tipe0 nor tipe1 are: TipeRef, TupleTipe, TupleIndexTipe, RecordTipe
	if lt0, ok := tipe0.(*ListTipe); ok {
		if lt1, ok := tipe1.(*ListTipe); ok {
			tc.unify(lt0.Tipe, lt1.Tipe)
			return
		}
		if _, ok := tipe1.(*SimpleTipe); ok {
			tc.error("can't use simple type as list type")
			return
		}
		panic("TODO")
		return
	}
	if _, ok := tipe1.(*ListTipe); ok {
		// tipe1 is a ListTipe, so swap them and try again
		tc.unify(tipe1, tipe0)
		return
	}

	// neither tipe0 nor tipe1 are: TipeRef, TupleTipe, TupleIndexTipe, RecordTipe, ListTipe
	if ft0, ok := tipe0.(*FuncTipe); ok {
		if ft1, ok := tipe1.(*FuncTipe); ok {
			tc.unify(ft0.Domain, ft1.Domain)
			tc.unify(ft0.Range, ft1.Range)
			return
		}
		if at1, ok := tipe1.(*AmbiguousTipe); ok {
			tc.resolveAmbiTipe(at1, func(t Tipe) bool {
				m := tc.match(ft0, t)
				return m == Yes || m == Maybe
			})
			if len(at1.Tipes) == 1 {
				tc.unify(ft0, at1.Tipes[0])
				return
			}
			// TODO: How do we make sure we don't throw away info?
			return
		}
		// don't know how to unify a FuncTipe and whatever tipe1 is
		panic("TODO")
		return
	}
	if _, ok := tipe1.(*FuncTipe); ok {
		// tipe1 is a FuncTipe, so swap them and try again
		tc.unify(tipe1, tipe0)
		return
	}

	// neither tipe0 nor tipe1 are: TipeRef, TupleTipe, TupleIndexTipe, RecordTipe, ListTipe, FuncTipe
	if _, ok := tipe0.(*TipeVar); ok {
		if _, ok := tipe1.(*TipeVar); ok {
			// I guess there's nothing to do here?
			return
		}
		if _, ok := tipe1.(*SimpleTipe); ok {
			// I guess there's nothing to do here?
			return
		}
		// don't know how to unify a TipeVar and whatever tipe1 is
		panic("TODO")
		return
	}
	if _, ok := tipe1.(*TipeVar); ok {
		// tipe1 is a TipeVar, so swap them and try again
		tc.unify(tipe1, tipe0)
		return
	}

	// neither tipe0 nor tipe1 are: TipeRef, TupleTipe, TupleIndexTipe, RecordTipe, ListTipe, FuncTipe, TipeVar
	if t0, ok := tipe0.(*AmbiguousTipe); ok {
		tc.resolveAmbiTipe(t0, func(t Tipe) bool {
			m := tc.match(t, tipe1)
			return m == Yes || m == Maybe
		})
		if len(t0.Tipes) == 1 {
			tc.unify(t0.Tipes[0], tipe1)
			return
		}
		// TODO: How do we make sure we don't throw away info?
		return
	}
	if _, ok := tipe1.(*AmbiguousTipe); ok {
		// tipe1 is an AmbiguousTipe, so swap them and try again
		tc.unify(tipe1, tipe0)
		return
	}

	// neither tipe0 nor tipe1 are: TipeRef, TupleTipe, TupleIndexTipe, RecordTipe, ListTipe, FuncTipe, AmbiguousTipe, TipeVar
	panic("TODO")
	return
}

type Match int

const (
	No = iota
	Maybe
	Yes
)

func (tc *TipeChecker) match(tipe0 Tipe, tipe1 Tipe) Match {
	if tipe0 == Omega || tipe1 == Omega {
		return Maybe
	}

	if tipe0 == tipe1 {
		return Yes
	}

	if t0, ok := tipe0.(*TipeRef); ok {
		return tc.match(t0.ref, tipe1)
	}
	if t1, ok := tipe1.(*TipeRef); ok {
		return tc.match(tipe0, t1.ref)
	}

	if ambiTipe, ok := tipe0.(*AmbiguousTipe); ok {
		var maybe bool
		for _, t := range ambiTipe.Tipes {
			m := tc.match(t, tipe1)
			if m == Yes {
				return Yes
			}
			if m == Maybe {
				// don't return; keep looking in case we find a Yes
				maybe = true
			}
		}
		if maybe {
			// at least one of the matches was a Maybe
			return Maybe
		}
		// none were even a Maybe
		return No
	}
	if _, ok := tipe1.(*AmbiguousTipe); ok {
		// swap and try again
		return tc.match(tipe1, tipe0)
	}

	if t0, ok := tipe0.(*SimpleTipe); ok {
		if t1, ok := tipe1.(*SimpleTipe); ok {
			if t0 == t1 {
				return Yes
			}
			return No
		}
		// swap and try again
		return tc.match(tipe1, tipe0)
	}

	if t0, ok := tipe0.(*TupleIndexTipe); ok {
		if tuple, ok := t0.Tuple.(*TupleTipe); ok {
			if len(tuple.Tipes) != t0.Size { // TODO: do we even need this check here?
				// not really sure what to do in this situation
				// is it even possible to get here? how did it happen?
				panic("TODO")
				return No
			}
			return tc.match(tuple.Tipes[t0.Index], tipe1)
		}
		if ambiTipe, ok := t0.Tuple.(*AmbiguousTipe); ok {
			var maybe bool
			for _, t := range ambiTipe.Tipes {
				m := tc.match(&TupleIndexTipe{
					Index: t0.Index,
					Size:  t0.Size,
					Tuple: t,
				}, tipe1)
				if m == Yes {
					return Yes
				}
				if m == Maybe {
					maybe = true
				}
			}
			if maybe {
				// at least one of the matches was a Maybe
				return Maybe
			}
			// none matched
			return No
		}
		// unhandled tipe
		panic("TODO")
		return No
	}
	if _, ok := tipe1.(*TupleIndexTipe); ok {
		// swap and try again
		return tc.match(tipe1, tipe0)
	}

	if t0, ok := tipe0.(*FuncTipe); ok {
		switch t1 := tipe1.(type) {
		case *SimpleTipe, *TupleTipe, *RecordTipe, *ListTipe:
			return No
		case *FuncTipe:
			dMatch := tc.match(t0.Domain, t1.Domain)
			rMatch := tc.match(t0.Range, t1.Range)
			if dMatch == Yes && rMatch == Yes {
				return Yes
			}
			if dMatch == No || rMatch == No {
				return No
			}
			return Maybe
		default:
			tc.error("Chris missed a type in tc.match(Func): %T", t1)
			return No
		}
	}
	if _, ok := tipe1.(*FuncTipe); ok {
		// swap and try again
		return tc.match(tipe1, tipe0)
	}

	if t0, ok := tipe0.(*TupleTipe); ok {
		switch t1 := tipe1.(type) {
		case *SimpleTipe, *RecordTipe, *ListTipe, *FuncTipe:
			return No
		case *TupleTipe:
			if len(t0.Tipes) != len(t1.Tipes) {
				return No
			}
			var tupleMatch Match = Yes
			for i, elem0 := range t0.Tipes {
				elem1 := t1.Tipes[i]
				m := tc.match(elem0, elem1)
				if m == No {
					return No // any failures fail the whole tuple match
				}
				if m == Maybe {
					tupleMatch = Maybe // demote it to Maybe
				}
				// if m == Yes { do nothing }
			}
			return tupleMatch
		default:
			tc.error("Chris missed a type in tc.match(Tuple): %T", t1)
			return No
		}
	}

	if t0, ok := tipe0.(*ListTipe); ok {
		switch t1 := tipe1.(type) {
		case *SimpleTipe, *RecordTipe, *TupleTipe, *FuncTipe:
			return No
		case *ListTipe:
			return tc.match(t0.Tipe, t1.Tipe)
		default:
			tc.error("Chris missed a type in tc.match(List): %T", t1)
			return No
		}
	}

	panic("TODO")
	return No
}

// remove failed matches (keeping both Yes and Maybe)
func (tc *TipeChecker) resolveAmbiTipe(ambiTipe *AmbiguousTipe, match func(Tipe) bool) {
	var matchingTipes []Tipe

	for _, tipe := range ambiTipe.Tipes {
		if match(tipe) {
			matchingTipes = append(matchingTipes, tipe)
		}
	}

	ambiTipe.Tipes = matchingTipes

	if len(ambiTipe.Tipes) < 1 {
		tc.error("no matches found when resolving ambiguous type")
	}
}

func (tc *TipeChecker) finalizeTipes(someExpr Expr) {
	tc.setFinalTipe(someExpr)

	switch expr := someExpr.(type) {

	case *UnitExpr, *IntExpr, *BoolExpr, *LookupExpr, *ArgExpr:
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

	case *FuncExpr:
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
	expr.SetFinalTipe(tc.determineFinalTipe(expr.TipeRef(tc)))
}

func (tc *TipeChecker) determineFinalTipe(someTipe Tipe) Tipe {
	if someTipe == Omega {
		// We never did get any information about this tipe,
		// so just return Omega.
		return Omega
	}

	switch tipe := someTipe.(type) {

	case *SimpleTipe:
		return tipe

	case *TipeVar:
		return tipe

	case *TipeRef:
		return tc.determineFinalTipe(tipe.ref)

	case *AmbiguousTipe:
		if len(tipe.Tipes) != 1 {
			tc.error("failed to resolve ambiguous type")
			return nil
		}
		return tc.determineFinalTipe(tipe.Tipes[0])

	case *TupleTipe:
		var finalTipes []Tipe
		for _, subTipe := range tipe.Tipes {
			finalTipes = append(finalTipes, tc.determineFinalTipe(subTipe))
		}
		tipe.Tipes = finalTipes
		return tipe

	case *RecordTipe:
		var finalFields []RecordFieldTipe
		for _, f := range tipe.Fields {
			field := RecordFieldTipe{
				Name: f.Name,
				Tipe: tc.determineFinalTipe(f.Tipe),
			}
			finalFields = append(finalFields, field)
		}
		tipe.Fields = finalFields
		return tipe

	case *ListTipe:
		if !isNil(tipe.Tipe) {
			tipe.Tipe = tc.determineFinalTipe(tipe.Tipe)
		}
		return tipe

	case *FuncTipe:
		tipe.Domain = tc.determineFinalTipe(tipe.Domain)
		tipe.Range = tc.determineFinalTipe(tipe.Range)
		return tipe

	case *TupleIndexTipe:
		allegedTuple := tc.determineFinalTipe(tipe.Tuple)
		if tuple, ok := allegedTuple.(*TupleTipe); ok {
			if len(tuple.Tipes) != tipe.Size { // TODO: is this check actually needed here?
				tc.error("wrong sized tuple")
				return nil
			}
			// tuple is already finalized, so no need to finalize sub-tipes
			return tuple.Tipes[tipe.Index]
		}
		tc.error("failed to resolve target of tuple-index type to be a tuple type")
		return nil

	default:
		tc.error("failed to finalize type: %T", tipe)
		return nil
	}
}
