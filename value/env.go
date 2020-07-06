package value

import (
	"crisp/ast"
	"fmt"
)

type Env struct {
	parent   *Env
	bindings map[string]Value
}

// bindings will initially be thunks to allow for
// recursive or out-of-order declarations
type Thunk struct {
	Expr ast.Expr
}

func (th *Thunk) Class() Class    { return ThunkClass }
func (th *Thunk) Inspect() string { return fmt.Sprintf("%v", th.Expr) }

func NewEnv(parent *Env, bindings map[string]Value) *Env {
	return &Env{parent: parent, bindings: bindings}
}

var TopLevelEnv = NewEnv(nil, map[string]Value{})

func (e *Env) Get(name string) Value {
	val, ok := e.bindings[name]
	if ok {
		return val
	}

	if e.parent != nil {
		return e.parent.Get(name)
	}

	panic("[env error] not defined: " + name)
	return nil
}

func (e *Env) Set(name string, val Value) {
	e.bindings[name] = val
}
