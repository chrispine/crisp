package eval

import (
	"crisp/ast"
	"crisp/value"
	"fmt"
)

func Eval(env *value.Env, someExpr ast.Expr) value.Value {
	switch expr := someExpr.(type) {
	case *ast.LetExpr:
		if len(expr.Asserts) > 0 {
			// TODO check expr.Asserts
			panic("come on, Chris: time to implement LetExpr assert evaluation")
		}
		return Eval(expr.Env, expr.Expr)
	case *ast.IntExpr:
		return &value.Int{Value: expr.Value}
	}

	panic(fmt.Sprintf("unhandled Expr %v of type %T", someExpr, someExpr))
	return nil
}
