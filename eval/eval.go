package eval

import (
	"crisp/ast"
	"crisp/token"
	"errors"
	"fmt"
	"math"
	"reflect"
)

func Eval(env *Env, someExpr ast.Expr) (val Value, err error) {
	defer func() {
		if r := recover(); r != nil {
			err = r.(error)
		}
	}()

	return force(eval(env, someExpr)), nil
}

func force(maybeThunk Value) Value {
	for {
		switch expr := maybeThunk.(type) {
		case *Thunk:
			if expr.Val == nil {
				val := eval(expr.Env, expr.Expr)
				expr.Val = val
				// let's make sure we never have a thunk's val pointing to a thunk
				maybeThunk = force(val)
				expr.Val = maybeThunk
			} else {
				maybeThunk = expr.Val
			}
		default:
			val := maybeThunk
			return val
		}
	}
}

func eval(env *Env, someExpr ast.Expr) Value {
	var val Value

	switch expr := someExpr.(type) {
	case *ast.UnitExpr:
		val = Unit
	case *ast.IntExpr:
		val = evalIntExpr(env, expr)
	case *ast.FloatExpr:
		val = evalFloatExpr(env, expr)
	case *ast.BoolExpr:
		val = evalBoolExpr(env, expr)
	case *ast.LookupExpr:
		val = evalLookupExpr(env, expr)
	case *ast.UnopExpr:
		val = evalUnopExpr(env, expr)
	case *ast.BinopExpr:
		val = evalBinopExpr(env, expr)
	case *ast.UserFuncExpr:
		val = evalUserFuncExpr(env, expr)
	case *ast.NativeFuncExpr:
		val = evalNativeFuncExpr(env, expr)
	case *ast.TupleExpr:
		val = evalTupleExpr(env, expr)
	case *ast.RecordExpr:
		val = evalRecordExpr(env, expr)
	case *ast.ConsExpr:
		val = evalConsExpr(env, expr)
	case *ast.RecordLookupExpr:
		val = evalRecordLookupExpr(env, expr)
	case *ast.TupleDestructureExpr:
		val = evalTupleDestructure(env, expr)
	case *ast.ConsDestructureExpr:
		val = evalConsDestructure(env, expr)
	case *ast.AssertEqualExpr:
		val = evalAssertEqual(env, expr)
	case *ast.AssertListIsConsOrNilExpr:
		val = evalAssertListIsConsOrNilExpr(env, expr)
	case *ast.AssertAnyOfTheseSets:
		val = evalAssertAnyOfTheseSets(env, expr)
	case *ast.LetExpr:
		// LetExpr is special because it contains runtime assertions
		// that must be tested. (In a function or `case` statement,
		// these would determine which branch to follow, but that is
		// handled in apply(func, val).)
		var ok bool
		val, ok = evalLetExpr(env, expr, nil)
		// In this situation (unlike function and `case`), failure means crashing.
		if !ok {
			panic("Runtime Error: failed assertion in `let` expression")
		}
	default:
		panic(errors.New(fmt.Sprintf(
			"Runtime Error: unhandled expression %v of type %T", someExpr, someExpr)))
	}

	return val
}

func applyUser(fn *UserFunc, arg Value) Value {
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

func applyNative(fn *NativeFunc, arg Value) Value {
	return fn.f(force(arg))
}

func evalIntExpr(_ *Env, expr *ast.IntExpr) *Int {
	return &Int{Value: expr.Value}
}

func evalFloatExpr(_ *Env, expr *ast.FloatExpr) *Float {
	return &Float{Value: expr.Value}
}

func evalBoolExpr(_ *Env, expr *ast.BoolExpr) *Bool {
	if expr.Value {
		return True
	}
	return False
}

func evalLookupExpr(env *Env, expr *ast.LookupExpr) Value {
	maybeThunk := env.Get(expr.Depth, expr.Index)
	// I don't *think* we care that this might be a thunk, I'm just saying.
	return maybeThunk
}

func evalTupleExpr(env *Env, expr *ast.TupleExpr) *Tuple {
	tuple := &Tuple{}

	var values []Value

	for _, elem := range expr.Exprs {
		values = append(values, &Thunk{Env: env, Expr: elem})
	}

	tuple.Values = values

	return tuple
}

func evalRecordExpr(env *Env, expr *ast.RecordExpr) *Record {
	record := &Record{}

	for _, field := range expr.Fields {
		val := &Thunk{Env: env, Expr: field.Expr}
		record.Fields = append(record.Fields, RecordField{Name: field.Name, Value: val})
	}

	return record
}

func evalConsExpr(env *Env, expr *ast.ConsExpr) *Cons {
	if expr.IsNilList() {
		return Nil
	}

	cons := &Cons{}

	cons.Head = &Thunk{Env: env, Expr: expr.Head}
	cons.Tail = &Thunk{Env: env, Expr: expr.Tail}

	return cons
}

func evalRecordLookupExpr(env *Env, expr *ast.RecordLookupExpr) Value {
	record := force(eval(env, expr.Record)).(*Record)

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
	tuple := force(eval(env, expr.Tuple)).(*Tuple)
	// TODO: did I just eval the same expression multiple times, one for each element?
	// TODO: same question with cons destructuring and all assertion types, and record lookup

	return tuple.Values[expr.Index]
}

func evalConsDestructure(env *Env, expr *ast.ConsDestructureExpr) Value {
	cons := force(eval(env, expr.List)).(*Cons)

	if expr.IsHead {
		return cons.Head
	}

	return cons.Tail
}

func evalAssertEqual(env *Env, expr *ast.AssertEqualExpr) *Bool {
	lVal := force(eval(env, expr.LExpr))
	rVal := force(eval(env, expr.RExpr))

	if equal(lVal, rVal) {
		return True
	}
	return False
}

func evalAssertListIsConsOrNilExpr(env *Env, expr *ast.AssertListIsConsOrNilExpr) *Bool {
	cons := force(eval(env, expr.List)).(*Cons)

	if expr.IsNil {
		// so we're asserting cons IS nil
		if cons == Nil {
			return True
		}
		return False
	}
	// so we're asserting cons IS NOT nil
	if cons == Nil {
		return False
	}
	return True
}

func evalAssertAnyOfTheseSets(env *Env, expr *ast.AssertAnyOfTheseSets) *Bool {
	for _, set := range expr.AssertSets {
		allMatch := true

		for _, assert := range set {
			val := force(eval(env, assert))
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
	val := force(eval(env, expr.Expr))

	switch expr.FinalTipe() {
	case ast.IntTipe:
		if expr.Token.Type == token.Minus {
			return &Int{Value: -val.(*Int).Value}
		}
	case ast.FloatTipe:
		if expr.Token.Type == token.Minus {
			return &Float{Value: -val.(*Float).Value}
		}
	}

	panic(fmt.Sprintf("RuntimeError: illegal unop expr: %v", expr))
	return nil
}

func evalBinopExpr(env *Env, expr *ast.BinopExpr) Value {
	binopType := expr.Token.Type
	lTipe := expr.LExpr.FinalTipe()

	switch lTipe.(type) {

	case *ast.SimpleTipe:
		{
			switch lTipe {
			case ast.UnitTipe:
				switch binopType {
				case token.Equal:
					// Since there's only one value of UnitTipe, and type-checking passed,
					// this has to be true.
					// TODO: generate a warning during type-checking?
					return True
				}
			case ast.BoolTipe:
				l := force(eval(env, expr.LExpr)).(*Bool).Value

				switch binopType {
				case token.Equal:
					r := force(eval(env, expr.RExpr)).(*Bool).Value
					if l == r {
						return True
					}
					return False
				case token.And:
					// don't eval `r` if we don't have to
					if !l {
						return False
					}
					r := force(eval(env, expr.RExpr)).(*Bool).Value
					if r {
						return True
					}
					return False
				case token.Or:
					// don't eval `r` if we don't have to
					if l {
						return True
					}
					r := force(eval(env, expr.RExpr)).(*Bool).Value
					if r {
						return True
					}
					return False
				}
			case ast.IntTipe:
				l := force(eval(env, expr.LExpr)).(*Int).Value
				r := force(eval(env, expr.RExpr)).(*Int).Value

				switch binopType {
				case token.Equal:
					if l == r {
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
			case ast.FloatTipe:
				l := force(eval(env, expr.LExpr)).(*Float).Value
				r := force(eval(env, expr.RExpr)).(*Float).Value

				switch binopType {
				case token.Equal:
					if l == r {
						return True
					}
					return False
				case token.FLT:
					if l < r {
						return True
					} else {
						return False
					}
				case token.FLTE:
					if l <= r {
						return True
					} else {
						return False
					}
				case token.FGT:
					if l > r {
						return True
					} else {
						return False
					}
				case token.FGTE:
					if l >= r {
						return True
					} else {
						return False
					}
				case token.FPlus:
					return &Float{Value: l + r}
				case token.FMinus:
					return &Float{Value: l - r}
				case token.FMult:
					return &Float{Value: l * r}
				case token.FDiv:
					return &Float{Value: l / r}
				case token.FMod:
					m := math.Mod(l, r)
					if m < 0 { // awesomeMod! <3
						m += r
					}
					return &Float{Value: m}
				case token.FExp:
					return &Float{Value: math.Pow(l, r)}
				}
			}
		}
	case *ast.FuncTipe:
		switch binopType {
		case token.Equal:
			panic("functions equality is undefined")
			return nil
		case token.At:
			leftVal := force(eval(env, expr.LExpr))
			rightVal := &Thunk{Env: env, Expr: expr.RExpr}
			if nativeFunc, ok := leftVal.(*NativeFunc); ok {
				return applyNative(nativeFunc, rightVal)
			}
			return applyUser(leftVal.(*UserFunc), rightVal)
		case token.DblMult:
			// we don't need to force these
			leftVal := &Thunk{Env: env, Expr: expr.LExpr}
			rightVal := &Thunk{Env: env, Expr: expr.RExpr}
			return composeFuncs(leftVal,
				expr.LExpr.FinalTipe().(*ast.FuncTipe),
				rightVal,
				expr.RExpr.FinalTipe().(*ast.FuncTipe))
		case token.DblExp:
			// we don't need to force the function
			leftVal := &Thunk{Env: env, Expr: expr.LExpr}
			rightVal := force(eval(env, expr.RExpr)).(*Int)
			if rightVal.Value >= 0 {
				var composition Value = &Thunk{Env: TopLevelEnv, Expr: ast.IdentityExpr}

				// this is the tipe of f and g
				tipe := expr.LExpr.FinalTipe().(*ast.FuncTipe)

				for i := 0; i < rightVal.Value; i++ {
					composition = composeFuncs(composition, tipe, leftVal, tipe)
				}

				return composition
			}
		}
	case *ast.TupleTipe:
		switch binopType {
		case token.Equal:
			// Note: type checker already determined these tuples are the same tipe
			leftVal := eval(env, expr.LExpr)
			rightVal := eval(env, expr.RExpr)
			if equal(leftVal, rightVal) {
				return True
			}
			return False
		}
	case *ast.RecordTipe:
		switch binopType {
		case token.Equal:
			// Note: type checker already determined these records are the same tipe
			leftVal := eval(env, expr.LExpr)
			rightVal := eval(env, expr.RExpr)
			if equal(leftVal, rightVal) {
				return True
			}
			return False
		}
	case *ast.ListTipe:
		switch binopType {
		case token.Equal:
			// Note: type checker already determined these lists are the same tipe
			leftVal := eval(env, expr.LExpr)
			rightVal := eval(env, expr.RExpr)
			if equal(leftVal, rightVal) {
				return True
			}
			return False
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

	// Now we need to set the correct env for those thunks.
	// However, if this is a function call, the first binding
	// is a value we don't want to mess with, even if it is
	// a thunk. (If it is, the env on it is already correct.)
	skipNextBinding := maybeArg != nil
	for _, b := range bindings {
		if skipNextBinding {
			skipNextBinding = false
			continue
		}
		b.Value.(*Thunk).Env = newEnv
	}
	// Awesome, `newEnv` is looking good, and the thunks all know about it.

	// Next, we attempt to validate the assertions
	for _, assert := range expr.Asserts {
		val := force(eval(newEnv, assert)).(*Bool)
		if !val.Value {
			return nil, false
		}
	}

	return &Thunk{Env: newEnv, Expr: expr.Expr}, true
}

func evalUserFuncExpr(env *Env, expr *ast.UserFuncExpr) *UserFunc {
	return &UserFunc{
		Env:            env,
		FuncPieceExprs: expr.FuncPieceExprs,
	}
}

func evalNativeFuncExpr(_ *Env, expr *ast.NativeFuncExpr) *NativeFunc {
	return expr.Func.(*NativeFunc)
}

func equal(aVal Value, bVal Value) bool {
	switch a := force(aVal).(type) {

	case *Unit_:
		// if they are both unit types, they must be equal
		return true

	case *Bool:
		if b, ok := force(bVal).(*Bool); ok {
			return a.Value == b.Value
		}

	case *Int:
		if b, ok := force(bVal).(*Int); ok {
			return a.Value == b.Value
		}

	case *Tuple:
		if b, ok := force(bVal).(*Tuple); ok {
			// these should be the same tipe, so no need to check tuple sizes
			for i, elem := range a.Values {
				if !equal(elem, b.Values[i]) {
					return false
				}
			}
			return true
		}

	case *Record:
		if b, ok := force(bVal).(*Record); ok {
			// these should be the same tipe, so no need to check field names
			for i, elem := range a.Fields {
				if !equal(elem.Value, b.Fields[i].Value) {
					return false
				}
			}
			return true
		}

	case *Cons:
		if b, ok := force(bVal).(*Cons); ok {
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

func composeFuncs(f Value, fTipe *ast.FuncTipe, g Value, gTipe *ast.FuncTipe) *UserFunc {
	env := NewEnv(TopLevelEnv, []*Binding{
		{Name: fName, Value: f},
		{Name: gName, Value: g},
	})

	arg := &ast.ArgExpr{}
	arg.Code = "«ArgExpr»"
	arg.SetFinalTipe(gTipe.Domain)

	lookupF := &ast.LookupExpr{Name: fName, Depth: 1, Index: 0}
	lookupF.Code = "«" + fName + " 1,0»"
	lookupF.SetFinalTipe(fTipe)

	lookupG := &ast.LookupExpr{Name: gName, Depth: 1, Index: 1}
	lookupG.Code = "«" + gName + " 1,1»"
	lookupG.SetFinalTipe(gTipe)

	lookupArg := &ast.LookupExpr{Name: ast.ArgName, Depth: 0, Index: 0}
	lookupArg.Code = "«" + ast.ArgName + " 0,0»"
	lookupArg.SetFinalTipe(gTipe.Domain)

	gAtArg := &ast.BinopExpr{
		Token: token.AtToken,
		LExpr: lookupG,
		RExpr: lookupArg,
	}
	gAtArg.Code = "(" + lookupG.Code + " @ " + lookupArg.Code + ")"
	gAtArg.SetFinalTipe(gTipe.Range)

	fAtGAtArg := &ast.BinopExpr{
		Token: token.AtToken,
		LExpr: lookupF,
		RExpr: gAtArg,
	}
	fAtGAtArg.Code = "(" + lookupF.Code + " @ " + gAtArg.Code + ")"
	fAtGAtArg.SetFinalTipe(fTipe.Range)

	funcPiece := &ast.LetExpr{
		Env: &ast.ExprEnv{
			Parent: nil,
			Bindings: []*ast.ExprBinding{{
				Name: ast.ArgName,
				Expr: arg,
			}},
		},
		Expr: fAtGAtArg,
	}
	funcPiece.Code = "let {\n" + fAtGAtArg.Code + "\n}"
	funcPiece.SetFinalTipe(fTipe.Range)

	return &UserFunc{Env: env, FuncPieceExprs: []*ast.LetExpr{funcPiece}}
}

func isNil(i interface{}) bool {
	return i == nil || reflect.ValueOf(i).IsNil()
}
