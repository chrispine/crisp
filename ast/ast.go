package ast

import (
	"crisp/token"
)

type Expr interface {
	String() string
	expr()
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
	Name string
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
	Bindings map[string]Expr
	Asserts  []Expr
	Expr     Expr
}

func (e *LetExpr) expr()          {}
func (e *LetExpr) String() string { return "LetExpr" }

type FuncExpr struct {
	FuncPartExprs []*LetExpr
}

func (e *FuncExpr) expr()          {}
func (e *FuncExpr) String() string { return "FuncExpr" }
