package eval

import (
	"crisp/ast"
	"crisp/token"
	"crisp/value"
	"fmt"
)

func Eval(env *value.Env, someExpr ast.Expr) value.Value {
	switch expr := someExpr.(type) {
	case *ast.LetExpr:
		return evalLetExpr(env, expr)
	case *ast.IntExpr:
		return evalIntExpr(env, expr)
	case *ast.BoolExpr:
		return evalBoolExpr(env, expr)
	case *ast.UnopExpr:
		return evalUnopExpr(env, expr)
	case *ast.BinopExpr:
		return evalBinopExpr(env, expr)
	}

	panic(fmt.Sprintf("Runtime Error: unhandled Expr %v of type %T", someExpr, someExpr))
	return nil
}

func evalIntExpr(_ *value.Env, expr *ast.IntExpr) value.Value {
	return &value.Int{Value: expr.Value}
}

func evalBoolExpr(_ *value.Env, expr *ast.BoolExpr) value.Value {
	return &value.Bool{Value: expr.Value}
}

func evalUnopExpr(env *value.Env, expr *ast.UnopExpr) value.Value {
	someVal := Eval(env, expr.Expr)

	switch val := someVal.(type) {
	case *value.Int:
		if expr.Token.Type == token.Minus {
			return &value.Int{Value: -val.Value}
		}
	case *value.Bool:
		if expr.Token.Type == token.Not {
			return &value.Bool{Value: !val.Value}
		}
	}

	panic(fmt.Sprintf("RuntimeError: illegal unop expr: %v", expr))
	return nil
}

func evalBinopExpr(env *value.Env, expr *ast.BinopExpr) value.Value {
	someLeftVal := Eval(env, expr.LExpr)
	someRightVal := Eval(env, expr.RExpr)

	switch leftVal := someLeftVal.(type) {
	case *value.Int:
		rightVal, ok := someRightVal.(*value.Int)

		if ok {
			l := leftVal.Value
			r := rightVal.Value

			switch expr.Token.Type {
			case token.Plus:
				return &value.Int{Value: l + r}
			case token.Minus:
				return &value.Int{Value: l - r}
			case token.Mult:
				return &value.Int{Value: l * r}
			case token.Div:
				return &value.Int{Value: l / r}
			case token.Mod:
				return &value.Int{Value: l % r} // TODO: awesomeMod() <3
			case token.Exp:
				if r >= 0 {
					val := 1
					for i := 0; i < r; i++ {
						val *= l
					}
					return &value.Int{Value: val}
				}
			}
		}
	case *value.Bool:
		rightVal, ok := someRightVal.(*value.Bool)

		if ok {
			l := leftVal.Value
			r := rightVal.Value

			switch expr.Token.Type {
			case token.And:
				return &value.Bool{Value: l && r}
			case token.Or:
				return &value.Bool{Value: l || r}
			}
		}
	}

	panic(fmt.Sprintf("RuntimeError: illegal unop expr: %v", expr))
	return nil
}

func evalLetExpr(env *value.Env, expr *ast.LetExpr) value.Value {
	if len(expr.Asserts) > 0 {
		// TODO check expr.Asserts
		panic("come on, Chris: time to implement LetExpr assert evaluation")
	}
	return Eval(expr.Env, expr.Expr)
}
