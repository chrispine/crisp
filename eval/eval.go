package eval

import (
	"crisp/ast"
	"crisp/token"
	"errors"
	"fmt"
)

func Eval(env *Env, someExpr ast.Expr) (val Value, err error) {
	defer func() {
		if r := recover(); r != nil {
			err = r.(error)
		}
	}()

	return eval(env, someExpr, nil), nil
}

func eval(env *Env, someExpr ast.Expr, binding *Binding) Value {
	var val Value

	switch expr := someExpr.(type) {
	case *ast.UnitExpr:
		val = Unit
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
	case *ast.AssertAnyOfTheseSets:
		val = evalAssertAnyOfTheseSets(env, expr)
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

func apply(fn *Func, arg Value) Value {
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

func evalIntExpr(_ *Env, expr *ast.IntExpr) *Int {
	return &Int{Value: expr.Value}
}

func evalBoolExpr(_ *Env, expr *ast.BoolExpr) *Bool {
	if expr.Value {
		return True
	}
	return False
}

func evalLookupExpr(env *Env, expr *ast.LookupExpr) Value {
	maybeThunk := env.Get(expr.Depth, expr.Index)

	if thunk, ok := maybeThunk.(*Thunk); ok {
		// force the thunk
		return eval(thunk.Env, thunk.Expr, env.GetBinding(expr.Depth, expr.Index))
	}

	return maybeThunk
}

func evalTupleExpr(env *Env, expr *ast.TupleExpr, binding *Binding) *Tuple {
	tuple := &Tuple{}

	if binding != nil {
		binding.Value = tuple
	}

	var values []Value

	for _, elem := range expr.Exprs {
		values = append(values, eval(env, elem, nil))
	}

	tuple.Values = values

	return tuple
}

func evalRecordExpr(env *Env, expr *ast.RecordExpr, binding *Binding) *Record {
	record := &Record{}

	if binding != nil {
		binding.Value = record
	}

	for _, field := range expr.Fields {
		val := eval(env, field.Expr, nil)
		record.Fields = append(record.Fields, RecordField{Name: field.Name, Value: val})
	}

	return record
}

func evalConsExpr(env *Env, expr *ast.ConsExpr, binding *Binding) *Cons {
	if expr == ast.NilList {
		return Nil
	}

	cons := &Cons{}

	if binding != nil {
		binding.Value = cons
	}

	cons.Head = eval(env, expr.Head, nil)
	cons.Tail = eval(env, expr.Tail, nil).(*Cons)

	return cons
}

func evalRecordLookupExpr(env *Env, expr *ast.RecordLookupExpr) Value {
	record := eval(env, expr.Record, nil).(*Record)

	// TODO: optimize this to just use an index (once we have a post-type-check optimizer)
	for _, field := range record.Fields {
		if expr.Name == field.Name {
			return field.Value
		}
	}

	panic("illegal record name")
	return nil
}

func evalTupleDestructure(env *Env, expr *ast.TupleDestructureExpr) Value {
	tuple := eval(env, expr.Tuple, nil).(*Tuple)
	// TODO: did I just eval the same expression multiple times, one for each element?
	// TODO: same question with cons destructuring and all assertion types, and record lookup

	return tuple.Values[expr.Index]
}

func evalConsDestructure(env *Env, expr *ast.ConsDestructureExpr) Value {
	cons := eval(env, expr.List, nil).(*Cons)

	if expr.IsHead {
		return cons.Head
	}

	return cons.Tail
}

func evalAssertEqual(env *Env, expr *ast.AssertEqualExpr) *Bool {
	lVal := eval(env, expr.LExpr, nil)
	rVal := eval(env, expr.RExpr, nil)

	if equal(lVal, rVal) {
		return True
	}
	return False
}

func evalAssertListIsCons(env *Env, expr *ast.AssertListIsConsExpr) *Bool {
	cons := eval(env, expr.List, nil)

	if cons == Nil {
		return False
	}
	return True
}

func evalAssertListIsNil(env *Env, expr *ast.AssertListIsNilExpr) *Bool {
	cons := eval(env, expr.List, nil)

	if cons == Nil {
		return True
	}
	return False
}

func evalAssertAnyOfTheseSets(env *Env, expr *ast.AssertAnyOfTheseSets) *Bool {
	for _, set := range expr.AssertSets {
		allMatch := true

		for _, assert := range set {
			val := eval(env, assert, nil)
			if val == False {
				allMatch = false
				break
			}
		}

		if allMatch {
			return True
		}
	}
	// none of the sets matched
	return False
}

func evalUnopExpr(env *Env, expr *ast.UnopExpr) Value {
	val := eval(env, expr.Expr, nil)

	switch expr.FinalTipe() {
	case ast.IntTipe:
		if expr.Token.Type == token.Minus {
			return &Int{Value: -val.(*Int).Value}
		}
	}

	panic(fmt.Sprintf("RuntimeError: illegal unop expr: %v", expr))
	return nil
}

func evalBinopExpr(env *Env, expr *ast.BinopExpr) Value {
	binopType := expr.Token.Type
	someLeftVal := eval(env, expr.LExpr, nil)
	someRightVal := eval(env, expr.RExpr, nil)

	switch leftVal := someLeftVal.(type) { // TODO: use Tipe instead of (type) for dispatch
	case *Unit_:
		if rightVal, ok := someRightVal.(*Unit_); ok {
			// kind of silly, since there's literally only one value of tipe UnitTipe
			switch binopType {
			case token.Equal:
				if equal(leftVal, rightVal) {
					return True
				}
				return False // cannot reach this line
			}
		}
	case *Int:
		if rightVal, ok := someRightVal.(*Int); ok {
			l := leftVal.Value
			r := rightVal.Value

			switch binopType {
			case token.Equal:
				if equal(leftVal, rightVal) {
					return True
				}
				return False
			case token.LT:
				if l < r {
					return True
				} else {
					return False
				}
			case token.LTE:
				if l <= r {
					return True
				} else {
					return False
				}
			case token.GT:
				if l > r {
					return True
				} else {
					return False
				}
			case token.GTE:
				if l >= r {
					return True
				} else {
					return False
				}
			case token.Plus:
				return &Int{Value: l + r}
			case token.Minus:
				return &Int{Value: l - r}
			case token.Mult:
				return &Int{Value: l * r}
			case token.Div:
				return &Int{Value: l / r}
			case token.Mod:
				m := l % r
				if m < 0 { // awesomeMod! <3
					m += r
				}
				return &Int{Value: m}
			case token.Exp:
				if r >= 0 {
					val := 1
					for i := 0; i < r; i++ {
						val *= l
					}
					return &Int{Value: val}
				}
			}
		}
	case *Bool:
		if rightVal, ok := someRightVal.(*Bool); ok {
			l := leftVal.Value
			r := rightVal.Value

			switch binopType {
			case token.Equal:
				if equal(leftVal, rightVal) {
					return True
				}
				return False
			case token.And:
				if l && r {
					return True
				} else {
					return False
				}
			case token.Or:
				if l || r {
					return True
				} else {
					return False
				}
			}
		}
	case *Func:
		switch binopType {
		case token.Equal:
			panic("not allowed to see if two functions are equal (no notion of function equality)")
			return nil
		case token.At:
			return apply(leftVal, someRightVal)
		case token.Mult:
			if rightVal, ok := someRightVal.(*Func); ok {
				return composeFuncs(leftVal, rightVal)
			}
		case token.Exp:
			if rightVal, ok := someRightVal.(*Int); ok {
				if rightVal.Value >= 0 {
					composition := Identity

					for i := 0; i < rightVal.Value; i++ {
						composition = composeFuncs(composition, leftVal)
					}

					return composition
				}
			}
		}
	case *Tuple:
		if rightVal, ok := someRightVal.(*Tuple); ok {
			switch binopType {
			case token.Equal:
				// Note: type checker already determined these tuples are the same tipe
				if equal(leftVal, rightVal) {
					return True
				}
				return False
			}
		}
	case *Record:
		if rightVal, ok := someRightVal.(*Record); ok {
			switch binopType {
			case token.Equal:
				// Note: type checker already determined these records are the same tipe
				if equal(leftVal, rightVal) {
					return True
				}
				return False
			}
		}
	case *Cons:
		if rightVal, ok := someRightVal.(*Cons); ok {
			switch binopType {
			case token.Equal:
				if equal(leftVal, rightVal) {
					return True
				}
				return False
			}
		}
	}

	panic(fmt.Sprintf("RuntimeError: illegal binop expr: %v", binopType))
	return nil
}

func evalLetExpr(env *Env, expr *ast.LetExpr, maybeArg Value) (Value, bool) {
	// First, we create a new environment in which we can evaluate
	// assertions, bindings, and the actual expression. We do this
	// by binding to thunks, since we obviously can't bind to the
	// values we haven't evaluated yet.
	var bindings []*Binding

	for _, eb := range expr.Env.Bindings {
		bindings = append(bindings, &Binding{
			Name:  eb.Name,
			Value: &Thunk{Expr: eb.Expr},
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

	newEnv := NewEnv(env, bindings)

	for _, b := range bindings {
		if th, ok := b.Value.(*Thunk); ok {
			th.Env = newEnv
		}
	}
	// Awesome, `newEnv` is looking good, and the thunks all know about it.

	// Next, we attempt to validate the assertions
	for _, assert := range expr.Asserts {
		val := eval(newEnv, assert, nil)
		if !val.(*Bool).Value {
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

func evalFuncExpr(env *Env, expr *ast.FuncExpr) *Func {
	return &Func{
		Env:            env,
		FuncPieceExprs: expr.FuncPieceExprs,
	}
}

func equal(aVal Value, bVal Value) bool {
	switch a := aVal.(type) {

	case *Unit_:
		if _, ok := bVal.(*Unit_); ok {
			// if they are both unit types, they must be equal
			return true
		}

	case *Bool:
		if b, ok := bVal.(*Bool); ok {
			return a.Value == b.Value
		}

	case *Int:
		if b, ok := bVal.(*Int); ok {
			return a.Value == b.Value
		}

	case *Tuple:
		if b, ok := bVal.(*Tuple); ok {
			// these should be the same tipe, so no need to check tuple sizes
			for i, elem := range a.Values {
				if !equal(elem, b.Values[i]) {
					return false
				}
			}
			return true
		}

	case *Record:
		if b, ok := bVal.(*Record); ok {
			// these should be the same tipe, so no need to check field names
			for i, elem := range a.Fields {
				if !equal(elem.Value, b.Fields[i].Value) {
					return false
				}
			}
			return true
		}

	case *Cons:
		if b, ok := bVal.(*Cons); ok {
			// first check for Nils
			if a == Nil {
				if b == Nil {
					return true
				}
				return false
			}
			if b == Nil {
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

func composeFuncs(f *Func, g *Func) *Func {
	env := NewEnv(EmptyEnv, []*Binding{
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

	return &Func{Env: env, FuncPieceExprs: composedFuncPieceExprs}
}
