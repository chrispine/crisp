package value

import "fmt"

// Class is not the same as the Crisp type
// Class might be Tuple, while Crisp type might be Tuple<Int, Int, Bool>
type Class int

const (
	ErrorClass = iota
	IntClass
	BoolClass
	TupleClass
	ConsClass
	FuncClass
)

type Value interface {
	Class() Class
	Inspect() string
}

type Int struct {
	Value int
}

func (i *Int) Class() Class    { return IntClass }
func (i *Int) Inspect() string { return fmt.Sprintf("%d", i.Value) }
