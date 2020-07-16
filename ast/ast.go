package ast

import (
	"crisp/token"
	"strconv"
)

type Expr interface {
	TipeRef(tc *TipeChecker) *TipeRef
	SetFinalTipe(tipe Tipe)
	FinalTipe() Tipe
	String() string
	finalizeAndGetCode() string
}

/*
 *   IntExpr
 */

type IntExpr struct {
	Code      string
	tipeVar   *TipeRef
	finalTipe Tipe
	Value     int
}

func (e *IntExpr) FinalTipe() Tipe { return e.finalTipe }
func (e *IntExpr) String() string  { return e.Code }
func (e *IntExpr) SetFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.finalizeAndGetCode()
}
func (e *IntExpr) finalizeAndGetCode() string {
	e.Code = strconv.Itoa(e.Value)
	return e.Code
}
func (e *IntExpr) TipeRef(tc *TipeChecker) *TipeRef {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeRef()
	}
	return e.tipeVar
}

/*
 *   BoolExpr
 */

type BoolExpr struct {
	Code      string
	tipeVar   *TipeRef
	finalTipe Tipe
	Value     bool
}

func (e *BoolExpr) FinalTipe() Tipe { return e.finalTipe }
func (e *BoolExpr) String() string  { return e.Code }
func (e *BoolExpr) SetFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.finalizeAndGetCode()
}
func (e *BoolExpr) finalizeAndGetCode() string {
	if e.Value {
		e.Code = "true"
	} else {
		e.Code = "false"
	}
	return e.Code
}
func (e *BoolExpr) TipeRef(tc *TipeChecker) *TipeRef {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeRef()
	}
	return e.tipeVar
}

var TrueExpr = &BoolExpr{Value: true}
var FalseExpr = &BoolExpr{Value: false}

/*
 *   LookupExpr
 */

type LookupExpr struct {
	Code      string
	tipeVar   *TipeRef
	finalTipe Tipe
	Name      string
	Depth     int
	Index     int
	Env       *ExprEnv
}

func (e *LookupExpr) FinalTipe() Tipe { return e.finalTipe }
func (e *LookupExpr) String() string  { return e.Code }
func (e *LookupExpr) SetFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.finalizeAndGetCode()
}
func (e *LookupExpr) finalizeAndGetCode() string {
	e.Code = "«" + e.Name + " " + strconv.Itoa(e.Depth) + "," + strconv.Itoa(e.Index) + "»"
	return e.Code
}
func (e *LookupExpr) TipeRef(tc *TipeChecker) *TipeRef {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeRef()
	}
	return e.tipeVar
}

/*
 *   RecordLookupExpr
 */

type RecordLookupExpr struct {
	Code      string
	tipeVar   *TipeRef
	finalTipe Tipe
	Name      string
	Names     []string
	Partial   bool
	Record    Expr
}

func (e *RecordLookupExpr) FinalTipe() Tipe { return e.finalTipe }
func (e *RecordLookupExpr) String() string  { return e.Code }
func (e *RecordLookupExpr) SetFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.finalizeAndGetCode()
}
func (e *RecordLookupExpr) finalizeAndGetCode() string {
	e.Code = "«" + e.Record.finalizeAndGetCode() + ":" + e.Name + "»"
	return e.Code
}
func (e *RecordLookupExpr) TipeRef(tc *TipeChecker) *TipeRef {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeRef()
	}
	return e.tipeVar
}

/*
 *   UnopExpr
 */

type UnopExpr struct {
	Code      string
	tipeVar   *TipeRef
	finalTipe Tipe
	Token     token.Token // the unop token, e.g. !
	Expr      Expr
}

func (e *UnopExpr) FinalTipe() Tipe { return e.finalTipe }
func (e *UnopExpr) String() string  { return e.Code }
func (e *UnopExpr) SetFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.finalizeAndGetCode()
}
func (e *UnopExpr) finalizeAndGetCode() string {
	e.Code = "(" + e.Token.Literal + e.Expr.finalizeAndGetCode() + ")"
	return e.Code
}
func (e *UnopExpr) TipeRef(tc *TipeChecker) *TipeRef {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeRef()
	}
	return e.tipeVar
}

/*
 *   BinopExpr
 */

type BinopExpr struct {
	Code      string
	tipeVar   *TipeRef
	finalTipe Tipe
	Token     token.Token // the operator token, e.g. +
	LExpr     Expr
	RExpr     Expr
}

func (e *BinopExpr) FinalTipe() Tipe { return e.finalTipe }
func (e *BinopExpr) String() string  { return e.Code }
func (e *BinopExpr) SetFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.finalizeAndGetCode()
}
func (e *BinopExpr) finalizeAndGetCode() string {
	e.Code = "(" + e.LExpr.finalizeAndGetCode() + " " + e.Token.Literal + " " + e.RExpr.finalizeAndGetCode() + ")"
	return e.Code
}
func (e *BinopExpr) TipeRef(tc *TipeChecker) *TipeRef {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeRef()
	}
	return e.tipeVar
}

/*
 *   LetExpr
 */

type LetExpr struct {
	Code      string
	tipeVar   *TipeRef
	finalTipe Tipe
	Env       *ExprEnv
	Asserts   []Expr
	Expr      Expr
}

func (e *LetExpr) FinalTipe() Tipe { return e.finalTipe }
func (e *LetExpr) String() string  { return e.Code }
func (e *LetExpr) SetFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.finalizeAndGetCode()
}
func (e *LetExpr) finalizeAndGetCode() string {
	e.Code = "let {\n" + e.Expr.finalizeAndGetCode() + "\n}"
	return e.Code
}
func (e *LetExpr) TipeRef(tc *TipeChecker) *TipeRef {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeRef()
	}
	return e.tipeVar
}

/*
 *   TupleExpr
 */

type TupleExpr struct {
	Code      string
	tipeVar   *TipeRef
	finalTipe Tipe
	Exprs     []Expr
}

func (e *TupleExpr) FinalTipe() Tipe { return e.finalTipe }
func (e *TupleExpr) String() string  { return e.Code }
func (e *TupleExpr) SetFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.finalizeAndGetCode()
}
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
func (e *TupleExpr) TipeRef(tc *TipeChecker) *TipeRef {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeRef()
	}
	return e.tipeVar
}

/*
 *   UnitExpr
 */

type UnitExpr struct { // this is the single 0-tuple: `()`
	Code      string
	tipeVar   *TipeRef
	finalTipe Tipe
}

func (e *UnitExpr) FinalTipe() Tipe { return e.finalTipe }
func (e *UnitExpr) String() string  { return e.Code }
func (e *UnitExpr) SetFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.finalizeAndGetCode()
}
func (e *UnitExpr) finalizeAndGetCode() string {
	e.Code = "()"
	return e.Code
}
func (e *UnitExpr) TipeRef(tc *TipeChecker) *TipeRef {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeRef()
	}
	return e.tipeVar
}

var Unit = &UnitExpr{}

/*
 *   RecordExpr
 */

type RecordFieldExpr struct {
	Name string
	Expr Expr
}

type RecordExpr struct {
	Code      string
	tipeVar   *TipeRef
	finalTipe Tipe
	Fields    []RecordFieldExpr
	Partial   bool
}

func (e *RecordExpr) FinalTipe() Tipe { return e.finalTipe }
func (e *RecordExpr) String() string  { return e.Code }
func (e *RecordExpr) SetFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.finalizeAndGetCode()
}
func (e *RecordExpr) finalizeAndGetCode() string {
	e.Code = "{TODO: RecordExpr}"
	return e.Code
}
func (e *RecordExpr) TipeRef(tc *TipeChecker) *TipeRef {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeRef()
	}
	return e.tipeVar
}

/*
 *   ConsExpr
 */

type ConsExpr struct {
	Code      string
	tipeVar   *TipeRef
	finalTipe Tipe
	Head      Expr
	Tail      Expr
}

func (e *ConsExpr) FinalTipe() Tipe { return e.finalTipe }
func (e *ConsExpr) String() string  { return e.Code }
func (e *ConsExpr) SetFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.finalizeAndGetCode()
}
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
func (e *ConsExpr) TipeRef(tc *TipeChecker) *TipeRef {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeRef()
	}
	return e.tipeVar
}

/*
 *   FuncExpr
 */

type FuncExpr struct {
	Code           string
	tipeVar        *TipeRef
	finalTipe      Tipe
	FuncPieceExprs []*LetExpr
}

func (e *FuncExpr) FinalTipe() Tipe { return e.finalTipe }
func (e *FuncExpr) String() string  { return e.Code }
func (e *FuncExpr) SetFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.finalizeAndGetCode()
}
func (e *FuncExpr) finalizeAndGetCode() string {
	e.Code = "Func[" + strconv.Itoa(len(e.FuncPieceExprs)) + " pc]"
	return e.Code
}
func (e *FuncExpr) TipeRef(tc *TipeChecker) *TipeRef {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeRef()
	}
	return e.tipeVar
}

var NotExprEnv = &ExprEnv{
	Parent:   nil,
	Bindings: []*ExprBinding{{Name: ArgName, Expr: &ArgExpr{}}},
}
var NotExpr = &FuncExpr{
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
var IdentityExpr = &FuncExpr{
	FuncPieceExprs: []*LetExpr{
		{
			Env:  IdentityExprEnv,
			Expr: &LookupExpr{Name: ArgName, Env: IdentityExprEnv},
		},
	},
}

/*
 *   ArgExpr
 */

// This is a placeholder node that, during runtime, is replaced
// by the actual arg passed into the function. We need it for
// type-checking functions.
type ArgExpr struct {
	Code      string
	tipeVar   *TipeRef
	finalTipe Tipe
}

func (e *ArgExpr) FinalTipe() Tipe { return e.finalTipe }
func (e *ArgExpr) String() string  { return e.Code }
func (e *ArgExpr) SetFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.finalizeAndGetCode()
}
func (e *ArgExpr) finalizeAndGetCode() string {
	e.Code = "«ArgExpr»"
	return e.Code
}
func (e *ArgExpr) TipeRef(tc *TipeChecker) *TipeRef {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeRef()
	}
	return e.tipeVar
}

/*
 *   AssertEqualExpr
 */

type AssertEqualExpr struct {
	Code      string
	tipeVar   *TipeRef
	finalTipe Tipe
	LExpr     Expr
	RExpr     Expr
}

func (e *AssertEqualExpr) FinalTipe() Tipe { return e.finalTipe }
func (e *AssertEqualExpr) String() string  { return e.Code }
func (e *AssertEqualExpr) SetFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.finalizeAndGetCode()
}
func (e *AssertEqualExpr) finalizeAndGetCode() string {
	e.Code = "«TODO: AssertEqualExpr»"
	return e.Code
}
func (e *AssertEqualExpr) TipeRef(tc *TipeChecker) *TipeRef {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeRef()
	}
	return e.tipeVar
}

/*
 *   AssertListIsConsExpr
 */

type AssertListIsConsExpr struct {
	Code      string
	tipeVar   *TipeRef
	finalTipe Tipe
	List      Expr
}

func (e *AssertListIsConsExpr) FinalTipe() Tipe { return e.finalTipe }
func (e *AssertListIsConsExpr) String() string  { return e.Code }
func (e *AssertListIsConsExpr) SetFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.finalizeAndGetCode()
}
func (e *AssertListIsConsExpr) finalizeAndGetCode() string {
	e.Code = "«TODO: AssertListIsConsExpr»"
	return e.Code
}
func (e *AssertListIsConsExpr) TipeRef(tc *TipeChecker) *TipeRef {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeRef()
	}
	return e.tipeVar
}

/*
 *   AssertListIsNilExpr
 */

type AssertListIsNilExpr struct {
	Code      string
	tipeVar   *TipeRef
	finalTipe Tipe
	List      Expr
}

func (e *AssertListIsNilExpr) FinalTipe() Tipe { return e.finalTipe }
func (e *AssertListIsNilExpr) String() string  { return e.Code }
func (e *AssertListIsNilExpr) SetFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.finalizeAndGetCode()
}
func (e *AssertListIsNilExpr) finalizeAndGetCode() string {
	e.Code = "«TODO: AssertListIsNilExpr»"
	return e.Code
}
func (e *AssertListIsNilExpr) TipeRef(tc *TipeChecker) *TipeRef {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeRef()
	}
	return e.tipeVar
}

/*
 *   AssertAnyOfTheseSets
 */

type AssertAnyOfTheseSets struct {
	Code       string
	tipeVar    *TipeRef
	finalTipe  Tipe
	AssertSets [][]Expr
}

func (e *AssertAnyOfTheseSets) FinalTipe() Tipe { return e.finalTipe }
func (e *AssertAnyOfTheseSets) String() string  { return e.Code }
func (e *AssertAnyOfTheseSets) SetFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.finalizeAndGetCode()
}
func (e *AssertAnyOfTheseSets) finalizeAndGetCode() string {
	e.Code = "«TODO: AssertAnyOfTheseSets»"
	return e.Code
}
func (e *AssertAnyOfTheseSets) TipeRef(tc *TipeChecker) *TipeRef {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeRef()
	}
	return e.tipeVar
}

/*
 *   TupleDestructureExpr
 */

type TupleDestructureExpr struct {
	Code      string
	tipeVar   *TipeRef
	finalTipe Tipe
	Index     int
	Size      int
	Tuple     Expr
}

func (e *TupleDestructureExpr) FinalTipe() Tipe { return e.finalTipe }
func (e *TupleDestructureExpr) String() string  { return e.Code }
func (e *TupleDestructureExpr) SetFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.finalizeAndGetCode()
}
func (e *TupleDestructureExpr) finalizeAndGetCode() string {
	e.Code = "«TODO: TupleDestructureExpr»"
	return e.Code
}
func (e *TupleDestructureExpr) TipeRef(tc *TipeChecker) *TipeRef {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeRef()
	}
	return e.tipeVar
}

/*
 *   ConsDestructureExpr
 */

type ConsDestructureExpr struct {
	Code      string
	tipeVar   *TipeRef
	finalTipe Tipe
	IsHead    bool
	List      Expr
}

func (e *ConsDestructureExpr) FinalTipe() Tipe { return e.finalTipe }
func (e *ConsDestructureExpr) String() string  { return e.Code }
func (e *ConsDestructureExpr) SetFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.finalizeAndGetCode()
}
func (e *ConsDestructureExpr) finalizeAndGetCode() string {
	e.Code = "«TODO: ConsDestructureExpr»"
	return e.Code
}
func (e *ConsDestructureExpr) TipeRef(tc *TipeChecker) *TipeRef {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeRef()
	}
	return e.tipeVar
}
