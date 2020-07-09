package value

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

// bindings will briefly be thunks to allow for
// recursive or out-of-order declarations
type Thunk struct {
	Env  *Env
	Expr ast.Expr
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
	if depth < 0 {
		// So we're looking for things bound in the top level env.
		// This is how we encode `true` and `false`.
		if idx == ast.MaxInt {
			return True
		}
		if idx == ast.MinInt {
			return False
		}

		// It must just be an int, so we'll use the idx as the integer value.
		return &Int{idx}
	}
	if depth > 0 {
		return e.Parent.Get(depth-1, idx)
	}

	if idx == -1 {
		return e.Bindings[len(e.Bindings)-1].Value
	}
	return e.Bindings[idx].Value
}

func (e *Env) GetBinding(depth int, idx int) *Binding {
	if depth > 0 {
		return e.Parent.GetBinding(depth-1, idx)
	}

	if e != TopLevelEnv {
		return e.Bindings[idx]
	}

	panic("[env error] TopLevelEnv has no explicit bindings!")
	return nil
}

func (e *Env) Update(depth int, idx int, val Value) {
	if depth > 0 {
		e.Parent.Update(depth-1, idx, val)
		return
	}

	if e != TopLevelEnv {
		e.Bindings[idx].Value = val
		return
	}

	// How did we get here? Shouldn't even be possible.
	panic("[env update error] may not modify top level environment")
	return
}
