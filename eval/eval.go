package eval

import (
	"crisp/ast"
	"crisp/token"
	"crisp/value"
	"fmt"
)

func Eval(env *value.Env, someExpr ast.Expr) value.Value {
	switch expr := someExpr.(type) {
	case *ast.IntExpr:
		return evalIntExpr(env, expr)
	case *ast.BoolExpr:
		return evalBoolExpr(env, expr)
	case *ast.LookupExpr:
		return evalLookupExpr(env, expr)
	case *ast.UnopExpr:
		return evalUnopExpr(env, expr)
	case *ast.BinopExpr:
		return evalBinopExpr(env, expr)
	case *ast.LetExpr:
		return evalLetExpr(env, expr)
	case *ast.FuncExpr:
		return evalFuncExpr(env, expr)
	case *ast.TupleExpr:
		return evalTupleExpr(env, expr)
	case *ast.TupleDestructureExpr:
		return evalTupleDestructure(env, expr)
	}

	panic(fmt.Sprintf("Runtime Error: unhandled expression %v of type %T", someExpr, someExpr))
	return nil
}

func apply(fn *value.Func, arg value.Value) value.Value {
	// TODO: properly handle multiple function pieces
	letExpr := fn.FuncPartExprs[0]
	argBindings := []value.Binding{{Name: fn.ArgName, Value: arg}}
	argEnv := value.NewEnv(fn.Env, argBindings)

	return Eval(argEnv, letExpr)
}

func evalIntExpr(_ *value.Env, expr *ast.IntExpr) value.Value {
	return &value.Int{Value: expr.Value}
}

func evalBoolExpr(_ *value.Env, expr *ast.BoolExpr) value.Value {
	return &value.Bool{Value: expr.Value}
}

func evalLookupExpr(env *value.Env, expr *ast.LookupExpr) value.Value {
	maybeThunk := env.Get(expr.Name)

	thunk, ok := maybeThunk.(*value.Thunk)
	if ok {
		// force the thunk
		val := Eval(env, thunk.Expr)
		// store the value so we don't have to force it again later
		env.Update(expr.Name, val)
		return val
	}

	return maybeThunk
}

func evalTupleExpr(env *value.Env, expr *ast.TupleExpr) value.Value {
	var values []value.Value

	for _, elem := range expr.Exprs {
		values = append(values, Eval(env, elem))
	}

	return &value.Tuple{values}
}

func evalTupleDestructure(env *value.Env, expr *ast.TupleDestructureExpr) value.Value {
	tuple := Eval(env, expr.Tuple).(*value.Tuple) // TODO: should I check for this error?

	return tuple.Values[expr.Index]
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
			case token.Equal:
				if l == r {
					return value.True
				} else {
					return value.False
				}
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
			case token.Equal:
				if l == r {
					return value.True
				} else {
					return value.False
				}
			case token.And:
				if l && r {
					return value.True
				} else {
					return value.False
				}
			case token.Or:
				if l || r {
					return value.True
				} else {
					return value.False
				}
			}
		}
	case *value.Func:
		switch expr.Token.Type {
		case token.At:
			return apply(leftVal, someRightVal)
		}
	}

	panic(fmt.Sprintf("RuntimeError: illegal binop expr: %v", expr))
	return nil
}

func evalLetExpr(env *value.Env, expr *ast.LetExpr) value.Value {
	for _, assert := range expr.Asserts {
		val := Eval(env, assert)
		if !val.(*value.Bool).Value {
			panic("Runtime Error: failed assertion in `let` expression")
		}
	}

	var bindings []value.Binding

	for _, eb := range expr.Env.Bindings {
		bindings = append(bindings, value.Binding{eb.Name, &value.Thunk{Expr: eb.Expr}})
	}
	return Eval(value.NewEnv(env, bindings), expr.Expr)
}

func evalFuncExpr(env *value.Env, expr *ast.FuncExpr) value.Value {
	return &value.Func{
		Env:           env,
		ArgName:       expr.ArgName,
		FuncPartExprs: expr.FuncPartExprs,
	}
}
