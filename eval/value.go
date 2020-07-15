package eval

import (
	"crisp/ast"
	"fmt"
)

// Class is not the same as the Crisp type
// Class might be Tuple, while Crisp type might be Tuple<Int, Int, Bool>
type Class int

const (
	UnitClass = iota
	IntClass
	BoolClass
	FuncClass
	ThunkClass
	TupleClass
	RecordClass
	ConsClass
)

type Value interface {
	Class() Class
	Inspect() string
}

type Int struct {
	Value int
}

func (i *Int) Class() Class    { return IntClass }
func (i *Int) Inspect() string { return fmt.Sprintf("%v", i.Value) }

type Bool struct {
	Value bool
}

func (b *Bool) Class() Class    { return BoolClass }
func (b *Bool) Inspect() string { return fmt.Sprintf("%v", b.Value) }

var True = &Bool{true}
var False = &Bool{false}

type Unit_ struct{}

func (b *Unit_) Class() Class    { return UnitClass }
func (b *Unit_) Inspect() string { return "()" }

var Unit = &Unit_{}

type Tuple struct {
	Values []Value
}

func (t *Tuple) Class() Class    { return TupleClass }
func (t *Tuple) Inspect() string { return "INSPECTED_TUPLE" }

type RecordField struct {
	Name  string
	Value Value
}
type Record struct {
	Fields []RecordField
}

func (t *Record) Class() Class    { return RecordClass }
func (t *Record) Inspect() string { return "INSPECTED_RECORD" }

type Cons struct {
	Head Value
	Tail *Cons
}

var Nil = &Cons{}

func (c *Cons) Class() Class { return ConsClass }
func (c *Cons) Inspect() string {
	if c == Nil {
		return "[]"
	}
	list := c
	str := "["
	for {
		str += list.Head.Inspect()
		if list.Tail == Nil {
			return str + "]"
		}
		str += ", "
		list = list.Tail
	}
}

type Func struct {
	Env            *Env
	FuncPieceExprs []*ast.LetExpr
}

func (f *Func) Class() Class    { return FuncClass }
func (f *Func) Inspect() string { return "INSPECTED_FUNC" }

var Identity = &Func{
	Env: EmptyEnv,
	FuncPieceExprs: []*ast.LetExpr{{
		Env: &ast.ExprEnv{
			Parent: ast.TopLevelExprEnv,
			Bindings: []*ast.ExprBinding{{
				Name: ast.ArgName,
				Expr: &ast.ArgExpr{},
			}},
		},
		Expr: &ast.LookupExpr{Name: ast.ArgName},
	}},
}
