package ast

import (
	"crisp/token"
	"crisp/value"
)

type Expr interface {
	String() string
	expr()
}

type IntExpr struct {
	Value int
}

func (le *IntExpr) expr()          {}
func (le *IntExpr) String() string { return "IntExpr" }

type BoolExpr struct {
	Value bool
}

func (le *BoolExpr) expr()          {}
func (le *BoolExpr) String() string { return "BoolExpr" }

type UnopExpr struct {
	Token token.Token // the unop token, e.g. !
	Expr  Expr
}

func (le *UnopExpr) expr()          {}
func (le *UnopExpr) String() string { return "UnopExpr" }

type BinopExpr struct {
	Token token.Token // the operator token, e.g. +
	LExpr Expr
	RExpr Expr
}

func (le *BinopExpr) expr()          {}
func (le *BinopExpr) String() string { return "BinopExpr" }

type LetExpr struct {
	Env     *value.Env
	Asserts []Expr
	Expr    Expr
}

func (le *LetExpr) expr()          {}
func (le *LetExpr) String() string { return "LetExpr" }
