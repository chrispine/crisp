package ast

import (
	"crisp/token"
	"strconv"
)

type Expr interface {
	TipeVar(tc *TipeChecker) *TipeVar
	setFinalTipe(tipe Tipe)
	FinalTipe() Tipe
	String() string
	getCode() string
}

/*
 *   IntExpr
 */

type IntExpr struct {
	code      string
	tipeVar   *TipeVar
	finalTipe Tipe
	Value     int
}

func (e *IntExpr) FinalTipe() Tipe { return e.finalTipe }
func (e *IntExpr) String() string  { return e.code }
func (e *IntExpr) setFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.getCode()
}
func (e *IntExpr) getCode() string {
	e.code = strconv.Itoa(e.Value)
	return e.code
}
func (e *IntExpr) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
	}
	return e.tipeVar
}

/*
 *   BoolExpr
 */

type BoolExpr struct {
	code      string
	tipeVar   *TipeVar
	finalTipe Tipe
	Value     bool
}

func (e *BoolExpr) FinalTipe() Tipe { return e.finalTipe }
func (e *BoolExpr) String() string  { return e.code }
func (e *BoolExpr) setFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.getCode()
}
func (e *BoolExpr) getCode() string {
	if e.Value {
		e.code = "true"
	} else {
		e.code = "false"
	}
	return e.code
}
func (e *BoolExpr) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
	}
	return e.tipeVar
}

var TrueExpr = &BoolExpr{Value: true}
var FalseExpr = &BoolExpr{Value: false}

/*
 *   LookupExpr
 */

type LookupExpr struct {
	code      string
	tipeVar   *TipeVar
	finalTipe Tipe
	Name      string
	Depth     int
	Index     int
	Env       *ExprEnv
}

func (e *LookupExpr) FinalTipe() Tipe { return e.finalTipe }
func (e *LookupExpr) String() string  { return e.code }
func (e *LookupExpr) setFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.getCode()
}
func (e *LookupExpr) getCode() string {
	e.code = "«" + e.Name + " " + strconv.Itoa(e.Depth) + "," + strconv.Itoa(e.Index) + "»"
	return e.code
}
func (e *LookupExpr) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
	}
	return e.tipeVar
}

/*
 *   RecordLookupExpr
 */

type RecordLookupExpr struct {
	code      string
	tipeVar   *TipeVar
	finalTipe Tipe
	Name      string
	Names     []string
	Partial   bool
	Record    Expr
}

func (e *RecordLookupExpr) FinalTipe() Tipe { return e.finalTipe }
func (e *RecordLookupExpr) String() string  { return e.code }
func (e *RecordLookupExpr) setFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.getCode()
}
func (e *RecordLookupExpr) getCode() string {
	e.code = "«" + e.Record.getCode() + ":" + e.Name + "»"
	return e.code
}
func (e *RecordLookupExpr) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
	}
	return e.tipeVar
}

/*
 *   UnopExpr
 */

type UnopExpr struct {
	code      string
	tipeVar   *TipeVar
	finalTipe Tipe
	Token     token.Token // the unop token, e.g. !
	Expr      Expr
}

func (e *UnopExpr) FinalTipe() Tipe { return e.finalTipe }
func (e *UnopExpr) String() string  { return e.code }
func (e *UnopExpr) setFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.getCode()
}
func (e *UnopExpr) getCode() string {
	e.code = "(" + e.Token.Literal + e.Expr.getCode() + ")"
	return e.code
}
func (e *UnopExpr) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
	}
	return e.tipeVar
}

/*
 *   BinopExpr
 */

type BinopExpr struct {
	code      string
	tipeVar   *TipeVar
	finalTipe Tipe
	Token     token.Token // the operator token, e.g. +
	LExpr     Expr
	RExpr     Expr
}

func (e *BinopExpr) FinalTipe() Tipe { return e.finalTipe }
func (e *BinopExpr) String() string  { return e.code }
func (e *BinopExpr) setFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.getCode()
}
func (e *BinopExpr) getCode() string {
	e.code = "(" + e.LExpr.getCode() + " " + e.Token.Literal + " " + e.RExpr.getCode() + ")"
	return e.code
}
func (e *BinopExpr) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
	}
	return e.tipeVar
}

/*
 *   LetExpr
 */

type LetExpr struct {
	code      string
	tipeVar   *TipeVar
	finalTipe Tipe
	Env       *ExprEnv
	Asserts   []Expr
	Expr      Expr
}

func (e *LetExpr) FinalTipe() Tipe { return e.finalTipe }
func (e *LetExpr) String() string  { return e.code }
func (e *LetExpr) setFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.getCode()
}
func (e *LetExpr) getCode() string {
	e.code = "let {\n" + e.Expr.getCode() + "\n}"
	return e.code
}
func (e *LetExpr) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
	}
	return e.tipeVar
}

/*
 *   TupleExpr
 */

type TupleExpr struct {
	code      string
	tipeVar   *TipeVar
	finalTipe Tipe
	Exprs     []Expr
}

func (e *TupleExpr) FinalTipe() Tipe { return e.finalTipe }
func (e *TupleExpr) String() string  { return e.code }
func (e *TupleExpr) setFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.getCode()
}
func (e *TupleExpr) getCode() string {
	e.code = ""
	for i, elem := range e.Exprs {
		if i == 0 {
			e.code += "("
		} else {
			e.code += ", "
		}
		e.code += elem.getCode()
	}
	e.code += ")"
	return e.code
}
func (e *TupleExpr) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
	}
	return e.tipeVar
}

/*
 *   UnitExpr
 */

type UnitExpr struct { // this is the single 0-tuple: `()`
	code      string
	tipeVar   *TipeVar
	finalTipe Tipe
}

func (e *UnitExpr) FinalTipe() Tipe { return e.finalTipe }
func (e *UnitExpr) String() string  { return e.code }
func (e *UnitExpr) setFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.getCode()
}
func (e *UnitExpr) getCode() string {
	e.code = "()"
	return e.code
}
func (e *UnitExpr) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
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
	code      string
	tipeVar   *TipeVar
	finalTipe Tipe
	Fields    []RecordFieldExpr
	Partial   bool
}

func (e *RecordExpr) FinalTipe() Tipe { return e.finalTipe }
func (e *RecordExpr) String() string  { return e.code }
func (e *RecordExpr) setFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.getCode()
}
func (e *RecordExpr) getCode() string {
	e.code = "{TODO: RecordExpr}"
	return e.code
}
func (e *RecordExpr) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
	}
	return e.tipeVar
}

/*
 *   ConsExpr
 */

type ConsExpr struct {
	code      string
	tipeVar   *TipeVar
	finalTipe Tipe
	Head      Expr
	Tail      Expr
}

var NilList = &ConsExpr{}

func (e *ConsExpr) FinalTipe() Tipe { return e.finalTipe }
func (e *ConsExpr) String() string  { return e.code }
func (e *ConsExpr) setFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.getCode()
}
func (e *ConsExpr) getCode() string {
	if e == NilList {
		e.code = "[]"
		return e.code
	}

	list := e
	e.code = "["
	for {
		e.code += list.Head.getCode()
		if list.Tail == NilList {
			e.code += "]"
			return e.code
		}
		if tail, ok := list.Tail.(*ConsExpr); ok {
			e.code += ", "
			list = tail
		} else {
			e.code += "; " + list.Tail.getCode() + "]"
			return e.code
		}
	}
}
func (e *ConsExpr) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
	}
	return e.tipeVar
}

/*
 *   FuncExpr
 */

type FuncExpr struct {
	code           string
	tipeVar        *TipeVar
	finalTipe      Tipe
	FuncPieceExprs []*LetExpr
}

func (e *FuncExpr) FinalTipe() Tipe { return e.finalTipe }
func (e *FuncExpr) String() string  { return e.code }
func (e *FuncExpr) setFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.getCode()
}
func (e *FuncExpr) getCode() string {
	e.code = "Func[" + strconv.Itoa(len(e.FuncPieceExprs)) + " pc]"
	return e.code
}
func (e *FuncExpr) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
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

/*
 *   ArgExpr
 */

// This is a placeholder node that, during runtime, is replaced
// by the actual arg passed into the function. We need it for
// type-checking functions.
type ArgExpr struct {
	code      string
	tipeVar   *TipeVar
	finalTipe Tipe
}

func (e *ArgExpr) FinalTipe() Tipe { return e.finalTipe }
func (e *ArgExpr) String() string  { return e.code }
func (e *ArgExpr) setFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.getCode()
}
func (e *ArgExpr) getCode() string {
	e.code = "«ArgExpr»"
	return e.code
}
func (e *ArgExpr) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
	}
	return e.tipeVar
}

/*
 *   AssertEqualExpr
 */

type AssertEqualExpr struct {
	code      string
	tipeVar   *TipeVar
	finalTipe Tipe
	LExpr     Expr
	RExpr     Expr
}

func (e *AssertEqualExpr) FinalTipe() Tipe { return e.finalTipe }
func (e *AssertEqualExpr) String() string  { return e.code }
func (e *AssertEqualExpr) setFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.getCode()
}
func (e *AssertEqualExpr) getCode() string {
	e.code = "«TODO: AssertEqualExpr»"
	return e.code
}
func (e *AssertEqualExpr) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
	}
	return e.tipeVar
}

/*
 *   AssertListIsConsExpr
 */

type AssertListIsConsExpr struct {
	code      string
	tipeVar   *TipeVar
	finalTipe Tipe
	List      Expr
}

func (e *AssertListIsConsExpr) FinalTipe() Tipe { return e.finalTipe }
func (e *AssertListIsConsExpr) String() string  { return e.code }
func (e *AssertListIsConsExpr) setFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.getCode()
}
func (e *AssertListIsConsExpr) getCode() string {
	e.code = "«TODO: AssertListIsConsExpr»"
	return e.code
}
func (e *AssertListIsConsExpr) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
	}
	return e.tipeVar
}

/*
 *   AssertListIsNilExpr
 */

type AssertListIsNilExpr struct {
	code      string
	tipeVar   *TipeVar
	finalTipe Tipe
	List      Expr
}

func (e *AssertListIsNilExpr) FinalTipe() Tipe { return e.finalTipe }
func (e *AssertListIsNilExpr) String() string  { return e.code }
func (e *AssertListIsNilExpr) setFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.getCode()
}
func (e *AssertListIsNilExpr) getCode() string {
	e.code = "«TODO: AssertListIsNilExpr»"
	return e.code
}
func (e *AssertListIsNilExpr) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
	}
	return e.tipeVar
}

/*
 *   AssertAnyOfTheseSets
 */

type AssertAnyOfTheseSets struct {
	code       string
	tipeVar    *TipeVar
	finalTipe  Tipe
	AssertSets [][]Expr
}

func (e *AssertAnyOfTheseSets) FinalTipe() Tipe { return e.finalTipe }
func (e *AssertAnyOfTheseSets) String() string  { return e.code }
func (e *AssertAnyOfTheseSets) setFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.getCode()
}
func (e *AssertAnyOfTheseSets) getCode() string {
	e.code = "«TODO: AssertAnyOfTheseSets»"
	return e.code
}
func (e *AssertAnyOfTheseSets) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
	}
	return e.tipeVar
}

/*
 *   TupleDestructureExpr
 */

type TupleDestructureExpr struct {
	code      string
	tipeVar   *TipeVar
	finalTipe Tipe
	Index     int
	Size      int
	Tuple     Expr
}

func (e *TupleDestructureExpr) FinalTipe() Tipe { return e.finalTipe }
func (e *TupleDestructureExpr) String() string  { return e.code }
func (e *TupleDestructureExpr) setFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.getCode()
}
func (e *TupleDestructureExpr) getCode() string {
	e.code = "«TODO: TupleDestructureExpr»"
	return e.code
}
func (e *TupleDestructureExpr) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
	}
	return e.tipeVar
}

/*
 *   ConsDestructureExpr
 */

type ConsDestructureExpr struct {
	code      string
	tipeVar   *TipeVar
	finalTipe Tipe
	IsHead    bool
	List      Expr
}

func (e *ConsDestructureExpr) FinalTipe() Tipe { return e.finalTipe }
func (e *ConsDestructureExpr) String() string  { return e.code }
func (e *ConsDestructureExpr) setFinalTipe(tipe Tipe) {
	e.finalTipe = tipe
	e.getCode()
}
func (e *ConsDestructureExpr) getCode() string {
	e.code = "«TODO: ConsDestructureExpr»"
	return e.code
}
func (e *ConsDestructureExpr) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
	}
	return e.tipeVar
}
