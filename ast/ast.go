package ast

import (
	"crisp/value"
)

type Expr interface {
	String() string
	expr()
}

type LetExpr struct {
	Env     *value.Env
	Asserts []Expr
	Expr    Expr
}

func (le *LetExpr) expr()          {}
func (le *LetExpr) String() string { return "LetExpr" }

type IntExpr struct {
	Value int
}

func (le *IntExpr) expr()          {}
func (le *IntExpr) String() string { return "IntExpr" }
