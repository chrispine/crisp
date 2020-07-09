package ast

import (
	"crisp/token"
)

type Expr interface {
	expr()
	String() string
}

type IntExpr struct {
	Value int
}

func (e *IntExpr) expr()          {}
func (e *IntExpr) String() string { return "IntExpr" }

type BoolExpr struct {
	Value bool
}

func (e *BoolExpr) expr()          {}
func (e *BoolExpr) String() string { return "BoolExpr" }

type LookupExpr struct {
	Name  string
	Depth int
	Index int
}

func (e *LookupExpr) expr()          {}
func (e *LookupExpr) String() string { return "LookupExpr" }

type UnopExpr struct {
	Token token.Token // the unop token, e.g. !
	Expr  Expr
}

func (e *UnopExpr) expr()          {}
func (e *UnopExpr) String() string { return "UnopExpr" }

type BinopExpr struct {
	Token token.Token // the operator token, e.g. +
	LExpr Expr
	RExpr Expr
}

func (e *BinopExpr) expr()          {}
func (e *BinopExpr) String() string { return "BinopExpr" }

type LetExpr struct {
	Env     *ExprEnv
	Asserts []Expr
	Expr    Expr
}

func (e *LetExpr) expr()          {}
func (e *LetExpr) String() string { return "LetExpr" }

type TupleExpr struct {
	Exprs []Expr
}

func (e *TupleExpr) expr()          {}
func (e *TupleExpr) String() string { return "TupleExpr" }

type ConsExpr struct {
	Head Expr
	Tail Expr
}

func (e *ConsExpr) expr()          {}
func (e *ConsExpr) String() string { return "ConsExpr" }

type FuncExpr struct {
	FuncPieceExprs []*LetExpr
}

func (e *FuncExpr) expr()          {}
func (e *FuncExpr) String() string { return "FuncExpr" }

type AssertEqualExpr struct {
	LExpr *LookupExpr
	RExpr Expr
}

func (e *AssertEqualExpr) expr()          {}
func (e *AssertEqualExpr) String() string { return "AssertEqualExpr" }

type AssertListIsConsExpr struct {
	List Expr
}

func (e *AssertListIsConsExpr) expr()          {}
func (e *AssertListIsConsExpr) String() string { return "AssertListIsConsExpr" }

type AssertListIsNilExpr struct {
	List Expr
}

func (e *AssertListIsNilExpr) expr()          {}
func (e *AssertListIsNilExpr) String() string { return "AssertListIsNilExpr" }
