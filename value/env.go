package value

import (
	"crisp/ast"
	"fmt"
	"strconv"
)

type Env struct {
	parent   *Env
	bindings []Binding
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
	Expr ast.Expr
}

func (th *Thunk) Class() Class    { return ThunkClass }
func (th *Thunk) Inspect() string { return fmt.Sprintf("%v", th.Expr) }

func NewEnv(parent *Env, bindings []Binding) *Env {
	if parent == nil {
		panic("Error: not allowed to create an Env with nil parent")
	}
	return &Env{parent: parent, bindings: bindings}
}

func (e *Env) Get(name string) Value {
	// check local bindings
	for _, b := range e.bindings {
		if b.Name == name {
			return b.Value
		}
	}

	// nope, let's see if a parent has it
	if e.parent != nil {
		return e.parent.Get(name)
	}

	if e == TopLevelEnv {
		return resolveTopLevelBinding(name)
	}

	// How did we get here? There's no parent, but this isn't the TopLevelEnv?
	panic("[very weird env error] not defined: " + name)
	return nil
}

func (e *Env) Update(name string, val Value) {
	// check local bindings
	for _, b := range e.bindings {
		if b.Name == name {
			b.Value = val
			return
		}
	}

	// nope, let's see if a parent has it
	if e.parent != nil {
		e.parent.Update(name, val)
		return
	}

	// How did we get here? Shouldn't even be possible.
	panic("[very weird env update error] not defined: " + name)
	return
}

func resolveTopLevelBinding(name string) Value {
	if name == "true" {
		return True
	}
	if name == "false" {
		return False
	}
	if i, err := strconv.Atoi(name); err == nil {
		return &Int{i}
	}

	panic("[env error] not defined: " + name)
	return nil
}
