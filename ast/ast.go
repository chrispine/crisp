package ast

import (
	"crisp/token"
	"strconv"
)

type Expr interface {
	String() string
	finalizeAndGetCode() string
}

type NativeCode interface {
	NativeCode()
}

/*
 *   IntExpr
 */

type IntExpr struct {
	Code  string
	Value int
}

func (e *IntExpr) String() string { return e.Code }
func (e *IntExpr) finalizeAndGetCode() string {
	e.Code = strconv.Itoa(e.Value)
	return e.Code
}

/*
 *   FloatExpr
 */

type FloatExpr struct {
	Code  string
	Value float64
}

func (e *FloatExpr) String() string { return e.Code }
func (e *FloatExpr) finalizeAndGetCode() string {
	e.Code = strconv.FormatFloat(e.Value, 'f', -1, 64)
	return e.Code
}

/*
 *   BoolExpr
 */

type BoolExpr struct {
	Code  string
	Value bool
}

func (e *BoolExpr) String() string { return e.Code }
func (e *BoolExpr) finalizeAndGetCode() string {
	if e.Value {
		e.Code = TrueName
	} else {
		e.Code = FalseName
	}
	return e.Code
}

var TrueExpr = &BoolExpr{Value: true}
var FalseExpr = &BoolExpr{Value: false}

/*
 *   LookupExpr
 */

type LookupExpr struct {
	Code  string
	Name  string
	Depth int
	Index int
	Env   *ExprEnv
}

func (e *LookupExpr) String() string { return e.Code }
func (e *LookupExpr) finalizeAndGetCode() string {
	e.Code = "«" + e.Name + " " + strconv.Itoa(e.Depth) + "," + strconv.Itoa(e.Index) + "»"
	return e.Code
}

/*
 *   RecordLookupExpr
 */

type RecordLookupExpr struct {
	Code    string
	Name    string
	Names   []string
	Partial bool
	Record  Expr
}

func (e *RecordLookupExpr) String() string { return e.Code }
func (e *RecordLookupExpr) finalizeAndGetCode() string {
	e.Code = "«" + e.Record.finalizeAndGetCode() + ":" + e.Name + "»"
	return e.Code
}

/*
 *   UnopExpr
 */

type UnopExpr struct {
	Code  string
	Token token.Token // the unop token, e.g. !
	Expr  Expr
}

func (e *UnopExpr) String() string { return e.Code }
func (e *UnopExpr) finalizeAndGetCode() string {
	e.Code = "(" + e.Token.Literal + e.Expr.finalizeAndGetCode() + ")"
	return e.Code
}

/*
 *   BinopExpr
 */

type BinopExpr struct {
	Code  string
	Token token.Token // the operator token, e.g. +
	LExpr Expr
	RExpr Expr
}

func (e *BinopExpr) String() string { return e.Code }
func (e *BinopExpr) finalizeAndGetCode() string {
	e.Code = "(" + e.LExpr.finalizeAndGetCode() + " " + e.Token.Literal + " " + e.RExpr.finalizeAndGetCode() + ")"
	return e.Code
}

/*
 *   LetExpr
 */

type LetExpr struct {
	Code    string
	Env     *ExprEnv
	Asserts []Expr
	Expr    Expr
}

func (e *LetExpr) String() string { return e.Code }
func (e *LetExpr) finalizeAndGetCode() string {
	e.Code = "let {\n" + e.Expr.finalizeAndGetCode() + "\n}"
	return e.Code
}

/*
 *   TupleExpr
 */

type TupleExpr struct {
	Code  string
	Exprs []Expr
}

func (e *TupleExpr) String() string { return e.Code }
func (e *TupleExpr) finalizeAndGetCode() string {
	e.Code = ""
	for i, elem := range e.Exprs {
		if i == 0 {
			e.Code += "("
		} else {
			e.Code += ", "
		}
		e.Code += elem.finalizeAndGetCode()
	}
	e.Code += ")"
	return e.Code
}

/*
 *   UnitExpr
 */

type UnitExpr struct { // this is the single 0-tuple: `()`
	Code string
}

func (e *UnitExpr) String() string { return e.Code }
func (e *UnitExpr) finalizeAndGetCode() string {
	e.Code = "()"
	return e.Code
}

var TheUnitExpr = &UnitExpr{}

/*
 *   RecordExpr
 */

type RecordFieldExpr struct {
	Name string
	Expr Expr
}

type RecordExpr struct {
	Code    string
	Fields  []RecordFieldExpr
	Partial bool
}

func (e *RecordExpr) String() string { return e.Code }
func (e *RecordExpr) finalizeAndGetCode() string {
	e.Code = "{TODO: RecordExpr}"
	return e.Code
}

/*
 *   ConsExpr
 */

type ConsExpr struct {
	Code string
	Head Expr
	Tail Expr
}

func (e *ConsExpr) String() string  { return e.Code }
func (e *ConsExpr) IsNilList() bool { return isNil(e.Head) && isNil(e.Tail) }
func (e *ConsExpr) finalizeAndGetCode() string {
	if e.IsNilList() {
		e.Code = "[]"
		return e.Code
	}

	list := e
	e.Code = "["
	for {
		e.Code += list.Head.finalizeAndGetCode()
		if tail, ok := list.Tail.(*ConsExpr); ok {
			if tail.IsNilList() {
				e.Code += "]"
				return e.Code
			}
			e.Code += ", "
			list = tail
		} else {
			e.Code += "; " + list.Tail.finalizeAndGetCode() + "]"
			return e.Code
		}
	}
}

/*
 *   UserFuncExpr
 */

type UserFuncExpr struct {
	Code           string
	FuncPieceExprs []*LetExpr
}

func (e *UserFuncExpr) String() string { return e.Code }
func (e *UserFuncExpr) finalizeAndGetCode() string {
	e.Code = "Func[" + strconv.Itoa(len(e.FuncPieceExprs)) + " pc]"
	return e.Code
}

// While the user did not define these, they certainly could have:
//   not(true ) -> false
//   not(false) -> true
//
//   identity(_) -> arg
// The resulting expressions would like basically like these:
var NotExprEnv = &ExprEnv{
	Parent:   nil,
	Bindings: []*ExprBinding{{Name: ArgName, Expr: &ArgExpr{}}},
}
var NotExpr = &UserFuncExpr{
	FuncPieceExprs: []*LetExpr{
		{
			Env: NotExprEnv,
			Asserts: []Expr{&AssertEqualExpr{
				LExpr: TrueExpr,
				RExpr: &LookupExpr{Name: ArgName, Env: NotExprEnv},
			}},
			Expr: FalseExpr,
		},
		{
			Env: NotExprEnv,
			Asserts: []Expr{&AssertEqualExpr{
				LExpr: FalseExpr,
				RExpr: &LookupExpr{Name: ArgName, Env: NotExprEnv},
			}},
			Expr: TrueExpr,
		},
	},
}

var IdentityExprEnv = &ExprEnv{
	Parent:   nil,
	Bindings: []*ExprBinding{{Name: ArgName, Expr: &ArgExpr{}}},
}
var IdentityExpr = &UserFuncExpr{
	FuncPieceExprs: []*LetExpr{
		{
			Env:  IdentityExprEnv,
			Expr: &LookupExpr{Name: ArgName, Env: IdentityExprEnv},
		},
	},
}

/*
 *   NativeFuncExpr
 */

// This is a placeholder node that, during runtime, is replaced
// by the actual arg passed into the function. We need it for
// type-checking functions.
type NativeFuncExpr struct {
	Code string
	Name string
	Func NativeCode
}

func (e *NativeFuncExpr) String() string { return e.Code }
func (e *NativeFuncExpr) finalizeAndGetCode() string {
	e.Code = "«Native: " + e.Name + "»"
	return e.Code
}

/*
 *   ArgExpr
 */

// This is a placeholder node that, during runtime, is replaced
// by the actual arg passed into the function. We need it for
// type-checking functions.
type ArgExpr struct {
	Code string
}

func (e *ArgExpr) String() string { return e.Code }
func (e *ArgExpr) finalizeAndGetCode() string {
	e.Code = "«ArgExpr»"
	return e.Code
}

/*
 *   AssertEqualExpr
 */

type AssertEqualExpr struct {
	Code  string
	LExpr Expr
	RExpr Expr
}

func (e *AssertEqualExpr) String() string { return e.Code }
func (e *AssertEqualExpr) finalizeAndGetCode() string {
	e.Code = "«TODO: AssertEqualExpr»"
	return e.Code
}

/*
 *   AssertListIsConsExpr
 */

type AssertListIsConsOrNilExpr struct {
	Code  string
	List  Expr
	IsNil bool
}

func (e *AssertListIsConsOrNilExpr) String() string { return e.Code }
func (e *AssertListIsConsOrNilExpr) finalizeAndGetCode() string {
	e.Code = "«TODO: AssertListIsConsOrNilExpr ("
	if e.IsNil {
		e.Code += "nil)»"
	} else {
		e.Code += "cons)»"
	}
	return e.Code
}

/*
 *   AssertAnyOfTheseSets
 */

type AssertAnyOfTheseSets struct {
	Code       string
	AssertSets [][]Expr
}

func (e *AssertAnyOfTheseSets) String() string { return e.Code }
func (e *AssertAnyOfTheseSets) finalizeAndGetCode() string {
	e.Code = "«TODO: AssertAnyOfTheseSets»"
	return e.Code
}

/*
 *   TupleDestructureExpr
 */

type TupleDestructureExpr struct {
	Code  string
	Index int
	Size  int
	Tuple Expr
}

func (e *TupleDestructureExpr) String() string { return e.Code }
func (e *TupleDestructureExpr) finalizeAndGetCode() string {
	e.Code = "«TODO: TupleDestructureExpr»"
	return e.Code
}

/*
 *   ConsDestructureExpr
 */

type ConsDestructureExpr struct {
	Code   string
	IsHead bool
	List   Expr
}

func (e *ConsDestructureExpr) String() string { return e.Code }
func (e *ConsDestructureExpr) finalizeAndGetCode() string {
	e.Code = "«TODO: ConsDestructureExpr»"
	return e.Code
}

/*
 *   code and names
 */

func finalizeCode(someExpr Expr) {
	someExpr.finalizeAndGetCode()

	switch expr := someExpr.(type) {

	case *UnitExpr, *IntExpr, *FloatExpr, *BoolExpr, *LookupExpr, *ArgExpr, *NativeFuncExpr:
		// nothing more to do

	case *LetExpr:
		for _, a := range expr.Asserts {
			finalizeCode(a)
		}

		for _, b := range expr.Env.Bindings {
			finalizeCode(b.Expr)
		}

		finalizeCode(expr.Expr)

	case *UnopExpr:
		finalizeCode(expr.Expr)

	case *BinopExpr:
		finalizeCode(expr.LExpr)
		finalizeCode(expr.RExpr)

	case *TupleExpr:
		for _, e := range expr.Exprs {
			finalizeCode(e)
		}

	case *RecordExpr:
		for _, f := range expr.Fields {
			finalizeCode(f.Expr)
		}

	case *ConsExpr:
		if !expr.IsNilList() {
			finalizeCode(expr.Head)
			finalizeCode(expr.Tail)
		}

	case *UserFuncExpr:
		for _, fp := range expr.FuncPieceExprs {
			finalizeCode(fp)
		}

	case *AssertEqualExpr:
		finalizeCode(expr.LExpr)
		finalizeCode(expr.RExpr)

	case *AssertListIsConsOrNilExpr:
		finalizeCode(expr.List)

	case *AssertAnyOfTheseSets:
		for _, set := range expr.AssertSets {
			for _, assert := range set {
				finalizeCode(assert)
			}
		}

	case *TupleDestructureExpr:
		finalizeCode(expr.Tuple)

	case *ConsDestructureExpr:
		finalizeCode(expr.List)

	case *RecordLookupExpr:
		finalizeCode(expr.Record)

	default:
		panic("failed to finalize type for expression")
	}
}

// This is the name that we bind to the argument when a function is called.
var ArgName = "arg"

// By making these "invalid" identifiers, we ensure no collisions with user code.
var IdentityName = "‡identity"
var UnitName = "‡unit"

// Names of other built-ins
var TrueName = "true"
var FalseName = "false"
var NotName = "!"
var IToFName = "i2f"
var FToIName = "f2i"
