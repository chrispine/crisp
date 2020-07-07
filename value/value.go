package value

import (
	"crisp/ast"
	"fmt"
)

// Class is not the same as the Crisp type
// Class might be Tuple, while Crisp type might be Tuple<Int, Int, Bool>
type Class int

const (
	IntClass = iota
	BoolClass
	FuncClass
	ThunkClass
	TupleClass
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

type Tuple struct {
	Values []Value
}

func (f *Tuple) Class() Class    { return TupleClass }
func (f *Tuple) Inspect() string { return "INSPECTED_TUPLE" }

type Func struct {
	Env           *Env
	ArgName       string
	FuncPartExprs []*ast.LetExpr
}

func (f *Func) Class() Class    { return FuncClass }
func (f *Func) Inspect() string { return "INSPECTED_FUNC" }
