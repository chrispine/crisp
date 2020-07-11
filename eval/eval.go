package eval

import (
	"crisp/ast"
	"crisp/token"
	"crisp/value"
	"fmt"
)

func Eval(env *value.Env, someExpr ast.Expr) value.Value {
	return eval(env, someExpr, nil)
}

func eval(env *value.Env, someExpr ast.Expr, binding *value.Binding) value.Value {
	var val value.Value

	switch expr := someExpr.(type) {
	case *ast.UnitExpr:
		val = value.Unit
	case *ast.IntExpr:
		val = evalIntExpr(env, expr)
	case *ast.BoolExpr:
		val = evalBoolExpr(env, expr)
	case *ast.LookupExpr:
		val = evalLookupExpr(env, expr)
	case *ast.UnopExpr:
		val = evalUnopExpr(env, expr)
	case *ast.BinopExpr:
		val = evalBinopExpr(env, expr)
	case *ast.FuncExpr:
		val = evalFuncExpr(env, expr)
	case *ast.TupleExpr:
		val = evalTupleExpr(env, expr, binding)
		binding = nil
	case *ast.RecordExpr:
		val = evalRecordExpr(env, expr, binding)
		binding = nil
	case *ast.ConsExpr:
		val = evalConsExpr(env, expr, binding)
		binding = nil
	case *ast.RecordLookupExpr:
		val = evalRecordLookupExpr(env, expr)
	case *ast.TupleDestructureExpr:
		val = evalTupleDestructure(env, expr)
	case *ast.ConsDestructureExpr:
		val = evalConsDestructure(env, expr)
	case *ast.AssertEqualExpr:
		val = evalAssertEqual(env, expr)
	case *ast.AssertListIsConsExpr:
		val = evalAssertListIsCons(env, expr)
	case *ast.AssertListIsNilExpr:
		val = evalAssertListIsNil(env, expr)
	case *ast.LetExpr:
		// LetExpr is special because it contains runtime assertions
		// that must be tested. (In a function or `case` statement,
		// these would determine which branch to follow, but that is
		// handled in apply(func, val).)
		var ok bool
		val, ok = evalLetExpr(env, expr, nil)
		if !ok {
			panic("Runtime Error: failed assertion in `let` expression")
		}
	default:
		panic(fmt.Sprintf("Runtime Error: unhandled expression %v of type %T", someExpr, someExpr))
	}

	if binding != nil {
		binding.Value = val
	}

	return val
}

func apply(fn *value.Func, arg value.Value) value.Value {
	// For each function-piece (each of which is a `let` expression)...
	for _, letExpr := range fn.FuncPieceExprs {
		// ...we evaluate the let expression to see if the assertions hold...
		val, ok := evalLetExpr(fn.Env, letExpr, arg)
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
		return eval(thunk.Env, thunk.Expr, env.GetBinding(expr.Depth, expr.Index))
	}

	return maybeThunk
}

func evalTupleExpr(env *value.Env, expr *ast.TupleExpr, binding *value.Binding) *value.Tuple {
	tuple := &value.Tuple{}

	if binding != nil {
		binding.Value = tuple
	}

	var values []value.Value

	for _, elem := range expr.Exprs {
		values = append(values, Eval(env, elem))
	}

	tuple.Values = values

	return tuple
}

func evalRecordExpr(env *value.Env, expr *ast.RecordExpr, binding *value.Binding) *value.Record {
	record := &value.Record{}

	if binding != nil {
		binding.Value = record
	}

	for _, field := range expr.Fields {
		val := Eval(env, field.Expr)
		record.Fields = append(record.Fields, value.RecordField{Name: field.Name, Value: val})
	}

	return record
}

func evalConsExpr(env *value.Env, expr *ast.ConsExpr, binding *value.Binding) *value.Cons {
	if expr == ast.NilList {
		return value.Nil
	}

	cons := &value.Cons{}

	if binding != nil {
		binding.Value = cons
	}

	cons.Head = Eval(env, expr.Head)
	cons.Tail = Eval(env, expr.Tail)

	return cons
}

func evalRecordLookupExpr(env *value.Env, expr *ast.RecordLookupExpr) value.Value {
	record := Eval(env, expr.Record).(*value.Record)

	// TODO: optimize this to just use an index (once we have types)
	for _, field := range record.Fields {
		if expr.Name == field.Name {
			return field.Value
		}
	}

	panic("illegal record name")
	return nil
}

func evalTupleDestructure(env *value.Env, expr *ast.TupleDestructureExpr) value.Value {
	tuple := Eval(env, expr.Tuple).(*value.Tuple)
	// TODO: did I just eval the same expression multiple times, one for each element?
	// TODO: same question with cons destructuring and all assertion types, and record lookup

	return tuple.Values[expr.Index]
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

	if cons == value.Nil {
		return value.False
	}
	return value.True
}

func evalAssertListIsNil(env *value.Env, expr *ast.AssertListIsNilExpr) *value.Bool {
	cons := Eval(env, expr.List)

	if cons == value.Nil {
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
	case *value.Unit_:
		if _, ok := someRightVal.(*value.Unit_); ok {
			return value.True
		}
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
		case token.Equal:
			panic("not allowed to see if two functions are equal (no notion of function equality)")
			return nil
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
	case *value.Tuple:
		if rightVal, ok := someRightVal.(*value.Tuple); ok {
			switch binopType {
			case token.Equal:
				// TODO: type checker should ensure they are the same size tuple, rather than a runtime check
				if len(leftVal.Values) != len(rightVal.Values) {
					return value.False
				}
				for i, v := range leftVal.Values {
					if !evalBinop(token.Equal, v, rightVal.Values[i]).(*value.Bool).Value {
						return value.False
					}
				}
				return value.True
			}
		}
	case *value.Record:
		if rightVal, ok := someRightVal.(*value.Record); ok {
			switch binopType {
			case token.Equal:
				// TODO: type checker should ensure they have the same fields, rather than a runtime check
				if len(leftVal.Fields) != len(rightVal.Fields) {
					return value.False
				}
				for i, f := range leftVal.Fields {
					if f.Name != rightVal.Fields[i].Name {
						return value.False
					}
					if !evalBinop(token.Equal, f.Value, rightVal.Fields[i].Value).(*value.Bool).Value {
						return value.False
					}
				}
				return value.True
			}
		}
	case *value.Cons:
		if rightVal, ok := someRightVal.(*value.Cons); ok {
			switch binopType {
			case token.Equal:
				// first check for Nils
				if leftVal == value.Nil {
					if rightVal == value.Nil {
						return value.True
					}
					return value.False
				}
				if rightVal == value.Nil {
					return value.False
				}
				if !evalBinop(token.Equal, leftVal.Head, rightVal.Head).(*value.Bool).Value {
					return value.False
				}
				return evalBinop(token.Equal, leftVal.Tail, rightVal.Tail)
			}
		}
	}

	if rightVal, ok := someRightVal.(*value.Func); ok {
		return apply(rightVal, someLeftVal)
	}

	panic(fmt.Sprintf("RuntimeError: illegal binop expr: %v", binopType))
	return nil
}

func evalLetExpr(env *value.Env, expr *ast.LetExpr, maybeArg value.Value) (value.Value, bool) {
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
		if len(bindings) > 0 {
			// this is where the actual argument is bound in the
			// evaluating environment
			bindings[0].Value = maybeArg
		} else {
			panic("should not be possible to call a function with no arg binding")
		}
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
		FuncPieceExprs: expr.FuncPieceExprs,
	}
}

var fName = "@f"
var gName = "@g"
var composedFuncPieceExprs = []*ast.LetExpr{
	{
		Env: &ast.ExprEnv{
			Parent: ast.TopLevelExprEnv,
			Bindings: []*ast.ExprBinding{{
				Name: ast.ArgName,
				Expr: &ast.ArgExpr{},
			}},
		},
		Expr: &ast.BinopExpr{
			Token: token.AtToken,
			LExpr: &ast.LookupExpr{Name: fName, Depth: 1, Index: 0},
			RExpr: &ast.BinopExpr{
				Token: token.AtToken,
				LExpr: &ast.LookupExpr{Name: gName, Depth: 1, Index: 1},
				RExpr: &ast.LookupExpr{Name: ast.ArgName, Depth: 0, Index: 0},
			},
		},
	},
}

func composeFuncs(f *value.Func, g *value.Func) *value.Func {
	env := value.NewEnv(value.EmptyEnv, []*value.Binding{
		{Name: fName, Value: f},
		{Name: gName, Value: g},
	})

	return &value.Func{Env: env, FuncPieceExprs: composedFuncPieceExprs}
}
