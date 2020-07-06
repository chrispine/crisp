package ast

import "strconv"

type ExprEnv struct {
	parent   *ExprEnv
	Bindings []ExprBinding
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
	if e.parent != nil {
		return e.parent.isDefined(name)
	}

	if e == TopLevelExprEnv {
		return topLevelDefined(name)
	}

	return false
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
