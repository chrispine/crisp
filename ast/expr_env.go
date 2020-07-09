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

var TopLevelExprEnv = &ExprEnv{}

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

	if e == TopLevelExprEnv {
		return topLevelDefined(name)
	}

	return false
}

func (e *ExprEnv) LookupIndices(name string) (int, int) {
	var depth int
	env := e

	for ; env != TopLevelExprEnv; depth++ {
		// check local bindings
		for idx, b := range env.Bindings {
			if b.Name == name {
				return depth, idx
			}
		}
		env = env.Parent
	}
	// A depth of -1 means this is a top-level binding.
	// TODO: create literals for these
	if name == "true" {
		return -1, MaxInt
	}
	if name == "false" {
		return -1, MinInt
	}
	if i, err := strconv.Atoi(name); err == nil {
		return -1, i
	}

	panic("LookupError: undefined identifier: " + name)
	return -1, -1
}

func topLevelDefined(name string) bool {
	if name == "true" {
		return true
	}
	if name == "false" {
		return true
	}
	if _, err := strconv.Atoi(name); err == nil {
		return true
	}

	return false
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

var TopLevelParseEnv = &ParseEnv{}

func (e *ParseEnv) isDefined(name string) bool {
	// check local bindings
	for _, b := range e.Bindings {
		if b.Name == name {
			return true
		}
	}

	// nope, let's see if a parent has it
	if e.parent != nil {
		return e.parent.isDefined(name)
	}

	if e == TopLevelParseEnv {
		return topLevelDefined(name)
	}

	return false
}

const MaxInt = int(^uint(0) >> 1)
const MinInt = -MaxInt - 1
