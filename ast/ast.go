package ast

import (
	"crisp/token"
)

type Expr interface {
	TipeVar() *TipeVar
	String() string
}

type IntExpr struct {
	tipeVar *TipeVar
	Value   int
}

func (e *IntExpr) String() string { return "IntExpr" }
func (e *IntExpr) TipeVar() *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = NewTipeVar()
	}
	return e.tipeVar
}

type BoolExpr struct {
	tipeVar *TipeVar
	Value   bool
}

func (e *BoolExpr) String() string { return "BoolExpr" }
func (e *BoolExpr) TipeVar() *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = NewTipeVar()
	}
	return e.tipeVar
}

var TrueExpr = &BoolExpr{Value: true}
var FalseExpr = &BoolExpr{Value: false}

type LookupExpr struct {
	tipeVar *TipeVar
	Name    string
	Depth   int
	Index   int
}

func (e *LookupExpr) String() string { return "LookupExpr" }
func (e *LookupExpr) TipeVar() *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = NewTipeVar()
	}
	return e.tipeVar
}

type RecordLookupExpr struct {
	tipeVar *TipeVar
	Name    string
	Record  Expr
}

func (e *RecordLookupExpr) String() string { return "RecordLookupExpr" }
func (e *RecordLookupExpr) TipeVar() *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = NewTipeVar()
	}
	return e.tipeVar
}

type UnopExpr struct {
	tipeVar *TipeVar
	Token   token.Token // the unop token, e.g. !
	Expr    Expr
}

func (e *UnopExpr) String() string { return "UnopExpr" }
func (e *UnopExpr) TipeVar() *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = NewTipeVar()
	}
	return e.tipeVar
}

type BinopExpr struct {
	tipeVar *TipeVar
	Token   token.Token // the operator token, e.g. +
	LExpr   Expr
	RExpr   Expr
}

func (e *BinopExpr) String() string { return "BinopExpr" }
func (e *BinopExpr) TipeVar() *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = NewTipeVar()
	}
	return e.tipeVar
}

type LetExpr struct {
	tipeVar *TipeVar
	Env     *ExprEnv
	Asserts []Expr
	Expr    Expr
}

func (e *LetExpr) String() string { return "LetExpr" }
func (e *LetExpr) TipeVar() *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = NewTipeVar()
	}
	return e.tipeVar
}

type TupleExpr struct {
	tipeVar *TipeVar
	Exprs   []Expr
}

func (e *TupleExpr) String() string { return "TupleExpr" }
func (e *TupleExpr) TipeVar() *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = NewTipeVar()
	}
	return e.tipeVar
}

type UnitExpr struct { // this is the single 0-tuple: `()`
	tipeVar *TipeVar
}

func (e *UnitExpr) String() string { return "UnitExpr" }
func (e *UnitExpr) TipeVar() *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = NewTipeVar()
	}
	return e.tipeVar
}

var Unit = &UnitExpr{}

type RecordFieldExpr struct {
	Name string
	Expr Expr
}

type RecordExpr struct {
	tipeVar     *TipeVar
	Fields      []RecordFieldExpr
	PartialLVal bool
}

func (e *RecordExpr) String() string { return "RecordExpr" }
func (e *RecordExpr) TipeVar() *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = NewTipeVar()
	}
	return e.tipeVar
}

type ConsExpr struct {
	tipeVar *TipeVar
	Head    Expr
	Tail    Expr
}

var NilList = &ConsExpr{}

func (e *ConsExpr) String() string { return "ConsExpr" }
func (e *ConsExpr) TipeVar() *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = NewTipeVar()
	}
	return e.tipeVar
}

type FuncExpr struct {
	tipeVar        *TipeVar
	FuncPieceExprs []*LetExpr
}

func (e *FuncExpr) String() string { return "FuncExpr" }
func (e *FuncExpr) TipeVar() *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = NewTipeVar()
	}
	return e.tipeVar
}

// This is a placeholder node that, during runtime, is replaced
// by the actual arg passed into the function. We need it for
// type-checking functions.
type ArgExpr struct {
	tipeVar *TipeVar
}

func (e *ArgExpr) String() string { return "ArgExpr" }
func (e *ArgExpr) TipeVar() *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = NewTipeVar()
	}
	return e.tipeVar
}

type AssertEqualExpr struct {
	tipeVar *TipeVar
	LExpr   Expr
	RExpr   Expr
}

func (e *AssertEqualExpr) String() string { return "AssertEqualExpr" }
func (e *AssertEqualExpr) TipeVar() *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = NewTipeVar()
	}
	return e.tipeVar
}

type AssertListIsConsExpr struct {
	tipeVar *TipeVar
	List    Expr
}

func (e *AssertListIsConsExpr) String() string { return "AssertListIsConsExpr" }
func (e *AssertListIsConsExpr) TipeVar() *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = NewTipeVar()
	}
	return e.tipeVar
}

type AssertListIsNilExpr struct {
	tipeVar *TipeVar
	List    Expr
}

func (e *AssertListIsNilExpr) String() string { return "AssertListIsNilExpr" }
func (e *AssertListIsNilExpr) TipeVar() *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = NewTipeVar()
	}
	return e.tipeVar
}

type TupleDestructureExpr struct {
	tipeVar *TipeVar
	Index   int
	Tuple   Expr
}

func (e *TupleDestructureExpr) String() string { return "TupleDestructureExpr" }
func (e *TupleDestructureExpr) TipeVar() *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = NewTipeVar()
	}
	return e.tipeVar
}

type ConsDestructureExpr struct {
	tipeVar *TipeVar
	IsHead  bool
	List    Expr
}

func (e *ConsDestructureExpr) String() string { return "ConsDestructureExpr" }
func (e *ConsDestructureExpr) TipeVar() *TipeVar {
	if e.tipeVar == nil {
		e.tipeVar = NewTipeVar()
	}
	return e.tipeVar
}
