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

func (t *Tuple) Class() Class { return TupleClass }
func (t *Tuple) Inspect() string {
	str := ""

	for i, elem := range t.Values {
		if i < 1 {
			str += "("
		} else {
			str += ", "
		}
		str += elem.Inspect()
	}
	str += ")"

	return str
}

type RecordField struct {
	Name  string
	Value Value
}
type Record struct {
	Fields []RecordField
}

func (t *Record) Class() Class { return RecordClass }
func (t *Record) Inspect() string {
	str := ""

	for i, field := range t.Fields {
		if i < 1 {
			str += "{"
		} else {
			str += ", "
		}
		str += field.Name + ": " + field.Value.Inspect()
	}
	str += "}"

	return str

}

type Cons struct {
	Head Value
	Tail Value
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
		str += force(list.Head).Inspect()
		list.Tail = force(list.Tail)
		if list.Tail == Nil {
			return str + "]"
		}
		str += ", "
		list = list.Tail.(*Cons)
	}
}

type Func struct {
	Env            *Env
	FuncPieceExprs []*ast.LetExpr
}

func (f *Func) Class() Class { return FuncClass }
func (f *Func) Inspect() string {
	return "«Function»"
}
