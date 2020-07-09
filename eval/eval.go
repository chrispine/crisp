package eval

import (
	"crisp/ast"
	"crisp/token"
	"crisp/value"
	"fmt"
	"reflect"
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
	case *ast.FuncExpr:
		return evalFuncExpr(env, expr)
	case *ast.TupleExpr:
		return evalTupleExpr(env, expr)
	case *ast.TupleDestructureExpr:
		return evalTupleDestructure(env, expr)
	case *ast.ConsExpr:
		return evalConsExpr(env, expr)
	case *ast.ConsDestructureExpr:
		return evalConsDestructure(env, expr)
	case *ast.AssertEqualExpr:
		return evalAssertEqual(env, expr)
	case *ast.AssertListIsConsExpr:
		return evalAssertListIsCons(env, expr)
	case *ast.AssertListIsNilExpr:
		return evalAssertListIsNil(env, expr)
	case *ast.LetExpr:
		// LetExpr is special because it contains runtime assertions
		// that must be tested. (In a function or `case` statement,
		// these would determine which branch to follow, but that is
		// handled in apply(func, val).)
		val, ok := evalLetExpr(env, expr, nil)
		if !ok {
			panic("Runtime Error: failed assertion in `let` expression")
		}
		return val
	}

	panic(fmt.Sprintf("Runtime Error: unhandled expression %v of type %T", someExpr, someExpr))
	return nil
}

func apply(fn *value.Func, arg value.Value) value.Value {
	argBinding := &value.Binding{Name: fn.ArgName, Value: arg}
	// For each function-piece (each of which is a `let` expression)...
	for _, letExpr := range fn.FuncPieceExprs {
		// ...we evaluate the let expression to see if the assertions hold...
		val, ok := evalLetExpr(fn.Env, letExpr, argBinding)
		// ...if not, we try the next one...
		if !ok {
			continue
		}
		// ...but if so, then this is the answer.
		return val
	}

	panic("Runtime Error: no matching function piece for function call")
}

func evalIntExpr(_ *value.Env, expr *ast.IntExpr) *value.Int {
	return &value.Int{Value: expr.Value}
}

func evalBoolExpr(_ *value.Env, expr *ast.BoolExpr) *value.Bool {
	if expr.Value {
		return value.True
	}
	return value.False
}

func evalLookupExpr(env *value.Env, expr *ast.LookupExpr) value.Value {
	maybeThunk := env.Get(expr.Depth, expr.Index)

	if thunk, ok := maybeThunk.(*value.Thunk); ok {
		// force the thunk
		val := Eval(thunk.Env, thunk.Expr) // TODO: , env.GetBinding(expr.Depth, expr.Index))
		// store the value so we don't have to force it again later
		env.Update(expr.Depth, expr.Index, val)
		return val
	}

	return maybeThunk
}

func evalTupleExpr(env *value.Env, expr *ast.TupleExpr) *value.Tuple {
	var values []value.Value

	for _, elem := range expr.Exprs {
		values = append(values, Eval(env, elem))
	}

	return &value.Tuple{Values: values}
}

func evalTupleDestructure(env *value.Env, expr *ast.TupleDestructureExpr) value.Value {
	tuple := Eval(env, expr.Tuple).(*value.Tuple)
	// TODO: did I just eval the same expression multiple times, one for each element?
	// TODO: same question with cons destructuring and all assertion types

	return tuple.Values[expr.Index]
}

func evalConsExpr(env *value.Env, expr *ast.ConsExpr) *value.Cons {
	var tail value.Value

	if !isNil(expr.Tail) {
		tail = Eval(env, expr.Tail)
	}

	return &value.Cons{
		Head: Eval(env, expr.Head),
		Tail: tail,
	}
}

func evalConsDestructure(env *value.Env, expr *ast.ConsDestructureExpr) value.Value {
	cons := Eval(env, expr.List).(*value.Cons)

	if expr.IsHead {
		return cons.Head
	}

	return cons.Tail
}

func evalAssertEqual(env *value.Env, expr *ast.AssertEqualExpr) *value.Bool {
	lVal := Eval(env, expr.LExpr)
	rVal := Eval(env, expr.RExpr)

	return evalBinop(token.Equal, lVal, rVal).(*value.Bool)
}

func evalAssertListIsCons(env *value.Env, expr *ast.AssertListIsConsExpr) *value.Bool {
	cons := Eval(env, expr.List)

	if isNil(cons) {
		return value.False
	}
	return value.True
}

func evalAssertListIsNil(env *value.Env, expr *ast.AssertListIsNilExpr) *value.Bool {
	cons := Eval(env, expr.List)

	if isNil(cons) {
		return value.True
	}
	return value.False
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
			if val.Value {
				return value.False
			}
			return value.True
		}
	}

	panic(fmt.Sprintf("RuntimeError: illegal unop expr: %v", expr))
	return nil
}

func evalBinopExpr(env *value.Env, expr *ast.BinopExpr) value.Value {
	someLeftVal := Eval(env, expr.LExpr)
	someRightVal := Eval(env, expr.RExpr)

	return evalBinop(expr.Token.Type, someLeftVal, someRightVal)
}
func evalBinop(binopType token.TokType, someLeftVal value.Value, someRightVal value.Value) value.Value {
	switch leftVal := someLeftVal.(type) {
	case *value.Int:
		if rightVal, ok := someRightVal.(*value.Int); ok {
			l := leftVal.Value
			r := rightVal.Value

			switch binopType {
			case token.Equal:
				if l == r {
					return value.True
				} else {
					return value.False
				}
			case token.NEq:
				if l == r {
					return value.False
				} else {
					return value.True
				}
			case token.LT:
				if l < r {
					return value.True
				} else {
					return value.False
				}
			case token.LTE:
				if l <= r {
					return value.True
				} else {
					return value.False
				}
			case token.GT:
				if l > r {
					return value.True
				} else {
					return value.False
				}
			case token.GTE:
				if l >= r {
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
				m := l % r
				if m < 0 { // awesomeMod! <3
					m += r
				}
				return &value.Int{Value: m}
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
		if rightVal, ok := someRightVal.(*value.Bool); ok {
			l := leftVal.Value
			r := rightVal.Value

			switch binopType {
			case token.Equal:
				if l == r {
					return value.True
				} else {
					return value.False
				}
			case token.NEq:
				if l == r {
					return value.False
				} else {
					return value.True
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
		switch binopType {
		case token.At:
			return apply(leftVal, someRightVal)
		case token.Mult:
			if rightVal, ok := someRightVal.(*value.Func); ok {
				return composeFuncs(leftVal, rightVal)
			}
		case token.Exp:
			if rightVal, ok := someRightVal.(*value.Int); ok {
				if rightVal.Value >= 0 {
					composition := value.Identity

					for i := 0; i < rightVal.Value; i++ {
						composition = composeFuncs(composition, leftVal)
					}

					return composition
				}
			}
		}
	}

	panic(fmt.Sprintf("RuntimeError: illegal binop expr: %v", binopType))
	return nil
}

func evalLetExpr(env *value.Env, expr *ast.LetExpr, maybeArg *value.Binding) (value.Value, bool) {
	// First, we create a new environment in which we can evaluate
	// assertions, bindings, and the actual expression. We do this
	// by binding to thunks, since we obviously can't bind to the
	// values we haven't evaluated yet.
	var bindings []*value.Binding

	for _, eb := range expr.Env.Bindings {
		bindings = append(bindings, &value.Binding{
			Name:  eb.Name,
			Value: &value.Thunk{Expr: eb.Expr},
		})
	}
	if maybeArg != nil {
		bindings = append(bindings, maybeArg)
	}

	newEnv := value.NewEnv(env, bindings)

	for _, b := range bindings {
		if th, ok := b.Value.(*value.Thunk); ok {
			th.Env = newEnv
		}
	}
	// Awesome, `newEnv` is looking good, and the thunks all know about it.

	// Next, we attempt to validate the assertions
	for _, assert := range expr.Asserts {
		val := Eval(newEnv, assert)
		if !val.(*value.Bool).Value {
			return nil, false
		}
	}

	// Finally, we force all of the thunks, so none remain when we return.
	for _, b := range newEnv.Bindings {
		Eval(newEnv, &ast.LookupExpr{Name: b.Name})
	}

	return Eval(newEnv, expr.Expr), true
}

func evalFuncExpr(env *value.Env, expr *ast.FuncExpr) *value.Func {
	return &value.Func{
		Env:            env,
		ArgName:        expr.ArgName,
		FuncPieceExprs: expr.FuncPieceExprs,
	}
}

func composeFuncs(f *value.Func, g *value.Func) *value.Func {
	argName := ast.GetArgName()
	fName := ast.GetArgName()
	gName := ast.GetArgName()

	env := value.NewEnv(value.EmptyEnv, []*value.Binding{
		{Name: fName, Value: f},
		{Name: gName, Value: g},
	})

	return &value.Func{
		Env:     env,
		ArgName: argName,
		FuncPieceExprs: []*ast.LetExpr{
			{
				Env: &ast.ExprEnv{
					Parent: &ast.ExprEnv{
						Bindings: []*ast.ExprBinding{
							{
								Name: argName,
								Expr: ast.Arg,
							},
						},
					},
				},
				Expr: &ast.BinopExpr{
					Token: token.AtToken,
					LExpr: &ast.LookupExpr{Name: fName, Depth: 1, Index: 0},
					RExpr: &ast.BinopExpr{
						Token: token.AtToken,
						LExpr: &ast.LookupExpr{Name: gName, Depth: 1, Index: 1},
						RExpr: &ast.LookupExpr{Name: argName, Depth: 0, Index: 0},
					},
				},
			},
		},
	}
}

func isNil(i interface{}) bool {
	return i == nil || reflect.ValueOf(i).IsNil()
}
