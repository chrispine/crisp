package ast

import (
	"crisp/parse_tree"
	"strconv"
)

// We need two different kinds of Envs here: one for binding
// parse tree expressions, and another for binding translated
// AST expressions. (And in the `value` package, there's a
// third one for binding values at runtime.)

// Go has no generics, so like... I'm just going to copy and
// paste this junk, and if you don't like it, well just imagine
// how I feel about it! If you know of a better way, please
// tell me. This is garbage.

type ExprEnv struct {
	Parent   *ExprEnv
	Bindings []*ExprBinding
}

type ExprBinding struct {
	Name string
	Expr Expr
}

func (e *ExprEnv) isDefined(name string) bool {
	// check local bindings
	for _, b := range e.Bindings {
		if b.Name == name {
			return true
		}
	}

	// nope, let's see if a parent has it
	if e.Parent != nil {
		return e.Parent.isDefined(name)
	}

	// we're at the top level environment,
	// so check for implicit top-level bindings (integers)
	_, err := strconv.Atoi(name)

	return err == nil
}

func (e *ExprEnv) Get(depth int, idx int) Expr {
	if depth > 0 {
		return e.Parent.Get(depth-1, idx)
	}

	return e.Bindings[idx].Expr
}

func (e *ExprEnv) LookupIndices(name string) Expr {
	var depth int
	env := e

	// check non-top-level environments
	for ; env.Parent != nil; depth++ {
		// check local bindings
		for idx, b := range env.Bindings {
			if b.Name == name {
				return &LookupExpr{Name: name, Depth: depth, Index: idx, Env: e}
			}
		}
		env = env.Parent
	}

	// if we got here, `env` is the top-level environment

	// check explicit top-level bindings
	for _, b := range env.Bindings {
		if b.Name == name {
			return b.Expr
		}
	}

	// check implicit top level bindings (integers)
	if i, err := strconv.Atoi(name); err == nil {
		return &IntExpr{Value: i}
	}

	// nothing was found
	// TODO: as this is an error, convert this to a more explicit error
	return nil
}

// "second verse, same as the first"

type ParseEnv struct {
	parent   *ParseEnv
	Bindings []*ParseBinding
}

type ParseBinding struct {
	Name string
	Expr parse_tree.Block
}
