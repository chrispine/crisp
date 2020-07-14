package eval

import (
	"crisp/ast"
	"crisp/token"
	"crisp/value"
	"errors"
	"fmt"
)

func Eval(env *value.Env, someExpr ast.Expr) (val value.Value, err error) {
	defer func() {
		if r := recover(); r != nil {
			err = r.(error)
		}
	}()

	return eval(env, someExpr, nil), nil
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
		panic(errors.New(fmt.Sprintf("Runtime Error: unhandled expression %v of type %T", someExpr, someExpr)))
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
		values = append(values, eval(env, elem, nil))
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
		val := eval(env, field.Expr, nil)
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

	cons.Head = eval(env, expr.Head, nil)
	cons.Tail = eval(env, expr.Tail, nil)

	return cons
}

func evalRecordLookupExpr(env *value.Env, expr *ast.RecordLookupExpr) value.Value {
	record := eval(env, expr.Record, nil).(*value.Record)

	// TODO: optimize this to just use an index (once we have a post-type-check optimizer)
	for _, field := range record.Fields {
		if expr.Name == field.Name {
			return field.Value
		}
	}

	panic("illegal record name")
	return nil
}

func evalTupleDestructure(env *value.Env, expr *ast.TupleDestructureExpr) value.Value {
	tuple := eval(env, expr.Tuple, nil).(*value.Tuple)
	// TODO: did I just eval the same expression multiple times, one for each element?
	// TODO: same question with cons destructuring and all assertion types, and record lookup

	return tuple.Values[expr.Index]
}

func evalConsDestructure(env *value.Env, expr *ast.ConsDestructureExpr) value.Value {
	cons := eval(env, expr.List, nil).(*value.Cons)

	if expr.IsHead {
		return cons.Head
	}

	return cons.Tail
}

func evalAssertEqual(env *value.Env, expr *ast.AssertEqualExpr) *value.Bool {
	lVal := eval(env, expr.LExpr, nil)
	rVal := eval(env, expr.RExpr, nil)

	if equal(lVal, rVal) {
		return value.True
	}
	return value.False
}

func evalAssertListIsCons(env *value.Env, expr *ast.AssertListIsConsExpr) *value.Bool {
	cons := eval(env, expr.List, nil)

	if cons == value.Nil {
		return value.False
	}
	return value.True
}

func evalAssertListIsNil(env *value.Env, expr *ast.AssertListIsNilExpr) *value.Bool {
	cons := eval(env, expr.List, nil)

	if cons == value.Nil {
		return value.True
	}
	return value.False
}

func evalUnopExpr(env *value.Env, expr *ast.UnopExpr) value.Value {
	val := eval(env, expr.Expr, nil)

	switch expr.FinalTipe() {
	case ast.IntTipe:
		if expr.Token.Type == token.Minus {
			return &value.Int{Value: -val.(*value.Int).Value}
		}
	}

	panic(fmt.Sprintf("RuntimeError: illegal unop expr: %v", expr))
	return nil
}

func evalBinopExpr(env *value.Env, expr *ast.BinopExpr) value.Value {
	binopType := expr.Token.Type
	someLeftVal := eval(env, expr.LExpr, nil)
	someRightVal := eval(env, expr.RExpr, nil)

	switch leftVal := someLeftVal.(type) { // TODO: use Tipe instead of (type) for dispatch
	case *value.Unit_:
		if rightVal, ok := someRightVal.(*value.Unit_); ok {
			// kind of silly, since there's literally only one value of tipe UnitTipe
			switch binopType {
			case token.Equal:
				if equal(leftVal, rightVal) {
					return value.True
				}
				return value.False // cannot reach this line
			}
		}
	case *value.Int:
		if rightVal, ok := someRightVal.(*value.Int); ok {
			l := leftVal.Value
			r := rightVal.Value

			switch binopType {
			case token.Equal:
				if equal(leftVal, rightVal) {
					return value.True
				}
				return value.False
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
				if equal(leftVal, rightVal) {
					return value.True
				}
				return value.False
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
				// Note: type checker already determined these tuples are the same tipe
				if equal(leftVal, rightVal) {
					return value.True
				}
				return value.False
			}
		}
	case *value.Record:
		if rightVal, ok := someRightVal.(*value.Record); ok {
			switch binopType {
			case token.Equal:
				// Note: type checker already determined these records are the same tipe
				if equal(leftVal, rightVal) {
					return value.True
				}
				return value.False
			}
		}
	case *value.Cons:
		if rightVal, ok := someRightVal.(*value.Cons); ok {
			switch binopType {
			case token.Equal:
				if equal(leftVal, rightVal) {
					return value.True
				}
				return value.False
			}
		}
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
		val := eval(newEnv, assert, nil)
		if !val.(*value.Bool).Value {
			return nil, false
		}
	}

	// Finally, we force all of the thunks, so none remain when we return.
	for _, b := range newEnv.Bindings {
		// we can skip eval() and call evalLookupExpr() directly
		evalLookupExpr(newEnv, &ast.LookupExpr{Name: b.Name})
	}

	return eval(newEnv, expr.Expr, nil), true
}

func evalFuncExpr(env *value.Env, expr *ast.FuncExpr) *value.Func {
	return &value.Func{
		Env:            env,
		FuncPieceExprs: expr.FuncPieceExprs,
	}
}

func equal(aVal value.Value, bVal value.Value) bool {
	switch a := aVal.(type) {

	case *value.Unit_:
		if _, ok := bVal.(*value.Unit_); ok {
			// if they are both unit types, they must be equal
			return true
		}

	case *value.Bool:
		if b, ok := bVal.(*value.Bool); ok {
			return a.Value == b.Value
		}

	case *value.Int:
		if b, ok := bVal.(*value.Int); ok {
			return a.Value == b.Value
		}

	case *value.Tuple:
		if b, ok := bVal.(*value.Tuple); ok {
			// these should be the same tipe, so no need to check tuple sizes
			for i, elem := range a.Values {
				if !equal(elem, b.Values[i]) {
					return false
				}
			}
			return true
		}

	case *value.Record:
		if b, ok := bVal.(*value.Record); ok {
			// these should be the same tipe, so no need to check field names
			for i, elem := range a.Fields {
				if !equal(elem.Value, b.Fields[i].Value) {
					return false
				}
			}
			return true
		}

	case *value.Cons:
		if b, ok := bVal.(*value.Cons); ok {
			// first check for Nils
			if a == value.Nil {
				if b == value.Nil {
					return true
				}
				return false
			}
			if b == value.Nil {
				return false
			}
			if !equal(a.Head, b.Head) {
				return false
			}
			return equal(a.Tail, b.Tail)
		}
	}
	// It should not be possible to get here. If we did get here,
	// it means we were trying to compare values of different tipes,
	// which should have been a type error.
	panic("runtime error: unhandled value in equality check")
	return false
}

var fName = "@f"
var gName = "@g"

func composeFuncs(f *value.Func, g *value.Func) *value.Func {
	env := value.NewEnv(value.EmptyEnv, []*value.Binding{
		{Name: fName, Value: f},
		{Name: gName, Value: g},
	})

	composedFuncPieceExprs := []*ast.LetExpr{{
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
		// TODO: create the Tipe
	}}

	return &value.Func{Env: env, FuncPieceExprs: composedFuncPieceExprs}
}
