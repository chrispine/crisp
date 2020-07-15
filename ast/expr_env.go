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

func (e *ExprEnv) Get(depth int, idx int) Expr {
	if depth > 0 {
		return e.Parent.Get(depth-1, idx)
	}

	return e.Bindings[idx].Expr
}

func (e *ExprEnv) LookupIndices(name string) Expr {
	var depth int
	env := e

	for ; env != TopLevelExprEnv; depth++ {
		// check local bindings
		for idx, b := range env.Bindings {
			if b.Name == name {
				return &LookupExpr{Name: name, Depth: depth, Index: idx, Env: e}
			}
		}
		env = env.Parent
	}
	// implicit top level bindings
	if name == "true" {
		return TrueExpr
	}
	if name == "false" {
		return FalseExpr
	}
	if name == "!" {
		return NotExpr
	}
	if name == IdentityName {
		return IdentityExpr
	}
	if i, err := strconv.Atoi(name); err == nil {
		return &IntExpr{Value: i}
	}

	// nothing was found
	// TODO: as this is an error, convert this to a more explicit error
	return nil
}

func topLevelDefined(name string) bool {
	if name == "true" {
		return true
	}
	if name == "false" {
		return true
	}
	if name == "!" {
		return true
	}
	if name == IdentityName {
		return true
	}
	if _, err := strconv.Atoi(name); err == nil {
		return true
	}

	return false
}

func TopLevelExprs() []Expr {
	return []Expr{
		TrueExpr,
		FalseExpr,
		IdentityExpr,
		NotExpr,
	}
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
