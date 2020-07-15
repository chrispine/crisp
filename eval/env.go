package eval

import (
	"crisp/ast"
	"fmt"
)

type Env struct {
	Parent   *Env
	Bindings []*Binding
}

type Binding struct {
	Name  string
	Value Value
}

// TopLevelEnv has special handling to bind ints and bools
var TopLevelEnv = &Env{}
var EmptyEnv = &Env{}

// bindings will initially be thunks to allow for
// recursive or out-of-order declarations
type Thunk struct {
	Env  *Env
	Expr ast.Expr
	Val  Value
}

func (th *Thunk) Class() Class    { return ThunkClass }
func (th *Thunk) Inspect() string { return fmt.Sprintf("%v", th.Expr) }

func NewEnv(parent *Env, bindings []*Binding) *Env {
	if parent == nil {
		panic("Error: not allowed to create an Env with nil parent")
	}
	return &Env{Parent: parent, Bindings: bindings}
}

func (e *Env) Get(depth int, idx int) Value {
	if depth > 0 {
		return e.Parent.Get(depth-1, idx)
	}

	binding := e.Bindings[idx]

	// if we have a forced thunk, let's just drop the thunk and use the (forced) value
	if th, ok := binding.Value.(*Thunk); ok {
		if !isNil(th.Val) {
			binding.Value = th.Val
		}
	}

	return binding.Value
}
