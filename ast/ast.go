package ast

import (
	"crisp/token"
)

type Expr interface {
	TipeVar(tc *TipeChecker) *TipeVar
	setFinalTipe(tipe Tipe)
	FinalTipe() Tipe
	String() string
}

type IntExpr struct {
	tipeVar   *TipeVar
	finalTipe Tipe
	Value     int
}

func (e *IntExpr) setFinalTipe(tipe Tipe) { e.finalTipe = tipe }
func (e *IntExpr) FinalTipe() Tipe        { return e.finalTipe }
func (e *IntExpr) String() string         { return "IntExpr" }
func (e *IntExpr) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
	}
	return e.tipeVar
}

type BoolExpr struct {
	tipeVar   *TipeVar
	finalTipe Tipe
	Value     bool
}

func (e *BoolExpr) setFinalTipe(tipe Tipe) { e.finalTipe = tipe }
func (e *BoolExpr) FinalTipe() Tipe        { return e.finalTipe }
func (e *BoolExpr) String() string         { return "BoolExpr" }
func (e *BoolExpr) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
	}
	return e.tipeVar
}

var TrueExpr = &BoolExpr{Value: true}
var FalseExpr = &BoolExpr{Value: false}

type LookupExpr struct {
	tipeVar   *TipeVar
	finalTipe Tipe
	Name      string
	Depth     int
	Index     int
	Env       *ExprEnv
}

func (e *LookupExpr) setFinalTipe(tipe Tipe) { e.finalTipe = tipe }
func (e *LookupExpr) FinalTipe() Tipe        { return e.finalTipe }
func (e *LookupExpr) String() string         { return "LookupExpr" }
func (e *LookupExpr) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
	}
	return e.tipeVar
}

type RecordLookupExpr struct {
	tipeVar   *TipeVar
	finalTipe Tipe
	Name      string
	Names     []string
	Partial   bool
	Record    Expr
}

func (e *RecordLookupExpr) setFinalTipe(tipe Tipe) { e.finalTipe = tipe }
func (e *RecordLookupExpr) FinalTipe() Tipe        { return e.finalTipe }
func (e *RecordLookupExpr) String() string         { return "RecordLookupExpr" }
func (e *RecordLookupExpr) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
	}
	return e.tipeVar
}

type UnopExpr struct {
	tipeVar   *TipeVar
	finalTipe Tipe
	Token     token.Token // the unop token, e.g. !
	Expr      Expr
}

func (e *UnopExpr) setFinalTipe(tipe Tipe) { e.finalTipe = tipe }
func (e *UnopExpr) FinalTipe() Tipe        { return e.finalTipe }
func (e *UnopExpr) String() string         { return "UnopExpr" }
func (e *UnopExpr) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
	}
	return e.tipeVar
}

type BinopExpr struct {
	tipeVar   *TipeVar
	finalTipe Tipe
	Token     token.Token // the operator token, e.g. +
	LExpr     Expr
	RExpr     Expr
}

func (e *BinopExpr) setFinalTipe(tipe Tipe) { e.finalTipe = tipe }
func (e *BinopExpr) FinalTipe() Tipe        { return e.finalTipe }
func (e *BinopExpr) String() string         { return "BinopExpr" }
func (e *BinopExpr) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
	}
	return e.tipeVar
}

type LetExpr struct {
	tipeVar   *TipeVar
	finalTipe Tipe
	Env       *ExprEnv
	Asserts   []Expr
	Expr      Expr
}

func (e *LetExpr) setFinalTipe(tipe Tipe) { e.finalTipe = tipe }
func (e *LetExpr) FinalTipe() Tipe        { return e.finalTipe }
func (e *LetExpr) String() string         { return "LetExpr" }
func (e *LetExpr) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
	}
	return e.tipeVar
}

type TupleExpr struct {
	tipeVar   *TipeVar
	finalTipe Tipe
	Exprs     []Expr
}

func (e *TupleExpr) setFinalTipe(tipe Tipe) { e.finalTipe = tipe }
func (e *TupleExpr) FinalTipe() Tipe        { return e.finalTipe }
func (e *TupleExpr) String() string         { return "TupleExpr" }
func (e *TupleExpr) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
	}
	return e.tipeVar
}

type UnitExpr struct { // this is the single 0-tuple: `()`
	tipeVar   *TipeVar
	finalTipe Tipe
}

func (e *UnitExpr) setFinalTipe(tipe Tipe) { e.finalTipe = tipe }
func (e *UnitExpr) FinalTipe() Tipe        { return e.finalTipe }
func (e *UnitExpr) String() string         { return "UnitExpr" }
func (e *UnitExpr) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
	}
	return e.tipeVar
}

var Unit = &UnitExpr{}

type RecordFieldExpr struct {
	Name string
	Expr Expr
}

type RecordExpr struct {
	tipeVar   *TipeVar
	finalTipe Tipe
	Fields    []RecordFieldExpr
	Partial   bool
}

func (e *RecordExpr) setFinalTipe(tipe Tipe) { e.finalTipe = tipe }
func (e *RecordExpr) FinalTipe() Tipe        { return e.finalTipe }
func (e *RecordExpr) String() string         { return "RecordExpr" }
func (e *RecordExpr) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
	}
	return e.tipeVar
}

type ConsExpr struct {
	tipeVar   *TipeVar
	finalTipe Tipe
	Head      Expr
	Tail      Expr
}

var NilList = &ConsExpr{}

func (e *ConsExpr) setFinalTipe(tipe Tipe) { e.finalTipe = tipe }
func (e *ConsExpr) FinalTipe() Tipe        { return e.finalTipe }
func (e *ConsExpr) String() string         { return "ConsExpr" }
func (e *ConsExpr) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
	}
	return e.tipeVar
}

type FuncExpr struct {
	tipeVar        *TipeVar
	finalTipe      Tipe
	FuncPieceExprs []*LetExpr
}

func (e *FuncExpr) setFinalTipe(tipe Tipe) { e.finalTipe = tipe }
func (e *FuncExpr) FinalTipe() Tipe        { return e.finalTipe }
func (e *FuncExpr) String() string         { return "FuncExpr" }
func (e *FuncExpr) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
	}
	return e.tipeVar
}

// This is a placeholder node that, during runtime, is replaced
// by the actual arg passed into the function. We need it for
// type-checking functions.
type ArgExpr struct {
	tipeVar   *TipeVar
	finalTipe Tipe
}

func (e *ArgExpr) setFinalTipe(tipe Tipe) { e.finalTipe = tipe }
func (e *ArgExpr) FinalTipe() Tipe        { return e.finalTipe }
func (e *ArgExpr) String() string         { return "ArgExpr" }
func (e *ArgExpr) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
	}
	return e.tipeVar
}

type AssertEqualExpr struct {
	tipeVar   *TipeVar
	finalTipe Tipe
	LExpr     Expr
	RExpr     Expr
}

func (e *AssertEqualExpr) setFinalTipe(tipe Tipe) { e.finalTipe = tipe }
func (e *AssertEqualExpr) FinalTipe() Tipe        { return e.finalTipe }
func (e *AssertEqualExpr) String() string         { return "AssertEqualExpr" }
func (e *AssertEqualExpr) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
	}
	return e.tipeVar
}

type AssertListIsConsExpr struct {
	tipeVar   *TipeVar
	finalTipe Tipe
	List      Expr
}

func (e *AssertListIsConsExpr) setFinalTipe(tipe Tipe) { e.finalTipe = tipe }
func (e *AssertListIsConsExpr) FinalTipe() Tipe        { return e.finalTipe }
func (e *AssertListIsConsExpr) String() string         { return "AssertListIsConsExpr" }
func (e *AssertListIsConsExpr) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
	}
	return e.tipeVar
}

type AssertListIsNilExpr struct {
	tipeVar   *TipeVar
	finalTipe Tipe
	List      Expr
}

func (e *AssertListIsNilExpr) setFinalTipe(tipe Tipe) { e.finalTipe = tipe }
func (e *AssertListIsNilExpr) FinalTipe() Tipe        { return e.finalTipe }
func (e *AssertListIsNilExpr) String() string         { return "AssertListIsNilExpr" }
func (e *AssertListIsNilExpr) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
	}
	return e.tipeVar
}

type TupleDestructureExpr struct {
	tipeVar   *TipeVar
	finalTipe Tipe
	Index     int
	Size      int
	Tuple     Expr
}

func (e *TupleDestructureExpr) setFinalTipe(tipe Tipe) { e.finalTipe = tipe }
func (e *TupleDestructureExpr) FinalTipe() Tipe        { return e.finalTipe }
func (e *TupleDestructureExpr) String() string         { return "TupleDestructureExpr" }
func (e *TupleDestructureExpr) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
	}
	return e.tipeVar
}

type ConsDestructureExpr struct {
	tipeVar   *TipeVar
	finalTipe Tipe
	IsHead    bool
	List      Expr
}

func (e *ConsDestructureExpr) setFinalTipe(tipe Tipe) { e.finalTipe = tipe }
func (e *ConsDestructureExpr) FinalTipe() Tipe        { return e.finalTipe }
func (e *ConsDestructureExpr) String() string         { return "ConsDestructureExpr" }
func (e *ConsDestructureExpr) TipeVar(tc *TipeChecker) *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = tc.newTipeVar()
	}
	return e.tipeVar
}
