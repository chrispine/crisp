package ast

import (
	"crisp/parse_tree"
	"crisp/token"
	"fmt"
	"reflect"
)

// translates parse trees into ASTs

type Translator struct {
	errors []string
}

func NewTranslator() *Translator { return &Translator{} }

// A Crisp program is treated as if it is the interior of a `let` expression.
func (tr *Translator) Translate(program *parse_tree.Program) Expr {
	lb := &parse_tree.LetBlock{
		Token: token.LetToken,
		Decls: program.Decls,
		Expr:  program.Expr,
	}

	return tr.translateLetBlock(TopLevelExprEnv, lb)
}

func (tr *Translator) Errors() []string {
	return tr.errors
}

func (tr *Translator) error(err string) {
	tr.errors = append(tr.errors, err)
	panic(err) // TODO: remove this line when translator is stable
}

func (tr *Translator) translateBlock(env *ExprEnv, blockTree parse_tree.Block) Expr {
	switch block := blockTree.(type) {
	case *parse_tree.JustExprBlock:
		return tr.translateJustExprBlock(env, block)
	case *parse_tree.LetBlock:
		return tr.translateLetBlock(env, block)
	case *parse_tree.FuncBlock:
		return tr.translateFuncBlock(env, block)
	case *parse_tree.CaseBlock:
		return tr.translateCaseBlock(env, block)
	case *parse_tree.ModuleBlock:
		return tr.translateModuleBlock(env, block)
	case *parse_tree.TupleBlock:
		return tr.translateTupleBlock(env, block)
	case *parse_tree.RecordBlock:
		return tr.translateRecordBlock(env, block)
	case *parse_tree.ConsBlock:
		return tr.translateConsBlock(env, block)
	case *TupleDestructureBlock:
		return tr.translateTupleDestructureBlock(env, block)
	case *RecordDestructureBlock:
		return tr.translateRecordDestructureBlock(env, block)
	case *ConsDestructureBlock:
		return tr.translateConsDestructureBlock(env, block)
	}

	tr.error(fmt.Sprintf("Translator Error: unhandled Block %v of type %T", blockTree, blockTree))
	return nil
}

func (tr *Translator) translateInline(env *ExprEnv, inlineTree parse_tree.Inline) Expr {
	switch inline := inlineTree.(type) {
	case *parse_tree.InlineID:
		if inline.Name == ArgName {
			// this is an argument binding, which will be appended
			// to the end of the bindings, hence (0, -1) meaning
			// "last element of this env"
			return &LookupExpr{Name: inline.Name, Depth: 0, Index: -1}
		}
		depth, idx := env.LookupIndices(inline.Name)
		return &LookupExpr{Name: inline.Name, Depth: depth, Index: idx}
	case *parse_tree.InlineUnopExpr:
		return &UnopExpr{
			Token: inline.Token,
			Expr:  tr.translateInline(env, inline.Expr),
		}
	case *parse_tree.InlineBinopExpr:
		if inline.Token.Type == token.Colon {
			// this is actually a record lookup
			name := inline.RExpr.(*parse_tree.InlineID).Name
			return &RecordLookupExpr{
				Name:   name,
				Record: tr.translateInline(env, inline.LExpr),
			}
		}
		return &BinopExpr{
			Token: inline.Token,
			LExpr: tr.translateInline(env, inline.LExpr),
			RExpr: tr.translateInline(env, inline.RExpr),
		}
	case *parse_tree.InlineFunc:
		return tr.translateInlineFunc(env, inline)
	case *parse_tree.InlineTuple:
		return tr.translateInlineTuple(env, inline)
	case *parse_tree.InlineRecord:
		return tr.translateInlineRecord(env, inline)
	case *parse_tree.InlineCons:
		return tr.translateInlineCons(env, inline)
	case *parse_tree.InlineNoMatch:
		panic("Invalid use of `_` outside of pattern matching.")
	}

	tr.error(fmt.Sprintf("Translator Error: unhandled Inline %v of type %T", inlineTree, inlineTree))
	return nil
}

func (tr *Translator) translateJustExprBlock(env *ExprEnv, block *parse_tree.JustExprBlock) Expr {
	return tr.translateInline(env, block.Expr)
}

type toAssert struct {
	// lhs and rhs are equal
	lhsEqual *parse_tree.InlineID
	rhsEqual parse_tree.Block
	// rhs is a cons cell
	listIsCons parse_tree.Block
	// rhs is a nil list
	listIsNil parse_tree.Block
}

func (tr *Translator) translateLetBlock(env *ExprEnv, block *parse_tree.LetBlock) *LetExpr {
	// We create two new envs: one temporary one to hold the
	// pre-translated parse tree expressions, and the one we
	// will give to the LetExpr to hold the post-translated
	// AST expressions.
	preEnv := &ParseEnv{parent: nil}
	postEnv := &ExprEnv{Parent: env}

	le := &LetExpr{Env: postEnv}

	// For each declaration (LVal = Expr), we break it down
	// into new bindings (which go into preEnv.Bindings)
	// and new assertions (which go into assertsToTranslate).
	// Note that we cannot translate the sub-expressions until
	// the env is complete, so we save them up and do that last.
	var preAsserts []toAssert

	for _, decl := range block.Decls {
		preAsserts = tr.partitionDecl(preEnv, postEnv, preAsserts, decl.LVal, decl.Expr, false)
	}

	// ensure binding names are unique
	uniqueNames := map[string]bool{}

	for _, b := range postEnv.Bindings {
		if _, ok := uniqueNames[b.Name]; ok {
			panic("[internal translator error] should not be possible to get two bindings with the same name")
		}

		uniqueNames[b.Name] = true
	}

	// Now that we know the name half of all bindings in preEnv,
	// we can proceed with translation of sub-expressions to populate
	// postEnv and le.Asserts.
	for _, ta := range preAsserts {
		var newAssert Expr

		switch {
		case !isNil(ta.listIsCons):
			newAssert = &AssertListIsConsExpr{List: tr.translateBlock(postEnv, ta.listIsCons)}
		case !isNil(ta.listIsNil):
			newAssert = &AssertListIsNilExpr{List: tr.translateBlock(postEnv, ta.listIsNil)}
		default:
			depth, idx := postEnv.LookupIndices(ta.lhsEqual.Name)

			newAssert = &AssertEqualExpr{
				LExpr: &LookupExpr{Name: ta.lhsEqual.Name, Depth: depth, Index: idx},
				RExpr: tr.translateBlock(postEnv, ta.rhsEqual),
			}
		}

		le.Asserts = append(le.Asserts, newAssert)
	}

	for i, bindBlock := range preEnv.Bindings {
		postEnv.Bindings[i].Expr = tr.translateBlock(postEnv, bindBlock.Expr)
	}

	le.Expr = tr.translateBlock(postEnv, block.Expr)

	return le
}

func (tr *Translator) partitionDecl(
	preEnv *ParseEnv,
	postEnv *ExprEnv,
	asserts []toAssert,
	lVal parse_tree.Inline,
	rhs parse_tree.Block,
	shadowing bool) []toAssert {

	switch lhs := lVal.(type) {
	case *parse_tree.InlineNoMatch:
		// `_ = expr`
		// do nothing
	case *parse_tree.InlineUnopExpr:
		// `$(lval) = expr`
		if lhs.Token.Type == token.Shadow {
			asserts = tr.partitionDecl(preEnv, postEnv, asserts, lhs.Expr, rhs, true)
		} else {
			tr.error("illegal l-value (should not be possible to get here)")
		}
	case *parse_tree.InlineID:
		// `x = expr`
		if !shadowing && postEnv.isDefined(lhs.Name) {
			// assert
			asserts = append(asserts, toAssert{lhsEqual: lhs, rhsEqual: rhs})
		} else {
			// binding
			preEnv.Bindings = append(preEnv.Bindings, &ParseBinding{Name: lhs.Name, Expr: rhs})
			postEnv.Bindings = append(postEnv.Bindings, &ExprBinding{Name: lhs.Name, Expr: nil})
			// that `Expr: nil` will be populated in just a bit with the translated `rhs`
		}
	case *parse_tree.InlineTuple:
		// `(a, b) = expr`
		// Note that we don't need to assert that `rhs` is a tuple, or what size of tuple,
		// because the type checker will flag that at compile time.
		// TODO: type checker needs to make sure these are the same size tuples
		for i, elem := range lhs.Exprs {
			td := &TupleDestructureBlock{i, rhs}
			asserts = tr.partitionDecl(preEnv, postEnv, asserts, elem, td, shadowing)
		}
	case *parse_tree.InlineRecord:
		// `{x: foo, y: bar} = expr`
		// TODO: type checker needs to make sure lhs has all the same fields rhs has, unless lhs.PartialLVal is true

		for k, v := range lhs.Elems {
			rd := &RecordDestructureBlock{k, rhs}
			asserts = tr.partitionDecl(preEnv, postEnv, asserts, v, rd, shadowing)
		}
	case *parse_tree.InlineCons:
		// `[a, b]` = expr
		// `[h; t]` = expr
		// `[    ]` = expr
		if lhs == parse_tree.InlineNil {
			asserts = append(asserts, toAssert{listIsNil: rhs})
		} else {
			asserts = append(asserts, toAssert{listIsCons: rhs})

			rhsHead := &ConsDestructureBlock{true, rhs}
			asserts = tr.partitionDecl(preEnv, postEnv, asserts, lhs.Head, rhsHead, shadowing)

			rhsTail := &ConsDestructureBlock{false, rhs}
			asserts = tr.partitionDecl(preEnv, postEnv, asserts, lhs.Tail, rhsTail, shadowing)
		}
	default:
		tr.error("illegal l-value (should not be possible to get here)")
	}

	return asserts
}

type TupleDestructureBlock struct {
	index int
	tuple parse_tree.Block
}

func (td *TupleDestructureBlock) BlockNode()           {}
func (td *TupleDestructureBlock) TokenLiteral() string { return "«TupleDestructureBlock»" }
func (td *TupleDestructureBlock) String() string       { return "«TupleDestructureBlock»" }

type RecordDestructureBlock struct {
	name   string
	record parse_tree.Block
}

func (rd *RecordDestructureBlock) BlockNode()           {}
func (rd *RecordDestructureBlock) TokenLiteral() string { return "«RecordDestructureBlock»" }
func (rd *RecordDestructureBlock) String() string       { return "«RecordDestructureBlock»" }

type ConsDestructureBlock struct {
	isHead bool
	list   parse_tree.Block
}

func (td *ConsDestructureBlock) BlockNode()           {}
func (td *ConsDestructureBlock) TokenLiteral() string { return "«ConsDestructureBlock»" }
func (td *ConsDestructureBlock) String() string       { return "«ConsDestructureBlock»" }

type TupleDestructureExpr struct {
	Index int
	Tuple Expr
}

func (e *TupleDestructureExpr) expr()          {}
func (e *TupleDestructureExpr) String() string { return "TupleDestructureExpr" }

type ConsDestructureExpr struct {
	IsHead bool
	List   Expr
}

func (e *ConsDestructureExpr) expr()          {}
func (e *ConsDestructureExpr) String() string { return "ConsDestructureExpr" }

func (tr *Translator) translateCaseBlock(env *ExprEnv, block *parse_tree.CaseBlock) *BinopExpr {
	return &BinopExpr{
		Token: token.AtToken,
		LExpr: tr.translateFuncBlock(env, block.Cases),
		RExpr: tr.translateInline(env, block.Expr),
	}
}

func (tr *Translator) translateModuleBlock(env *ExprEnv, block *parse_tree.ModuleBlock) *LetExpr {
	elems := map[string]parse_tree.Inline{}

	for _, ex := range block.Exports {
		elems[ex] = &parse_tree.InlineID{
			Token: token.Token{Type: token.ID, Literal: ex},
			Name:  ex,
		}
	}

	if len(elems) != len(block.Exports) {
		tr.error("illegal module definition: exports must be unique")
	}

	record := &parse_tree.InlineRecord{
		Token: token.Token{Type: token.LBrace, Literal: "{"},
		Elems: elems,
	}
	lit := &parse_tree.LetBlock{
		Token: token.LetToken,
		Decls: block.Decls,
		Expr: &parse_tree.JustExprBlock{
			Token: token.ExprBlockToken,
			Expr:  record,
		},
	}

	return tr.translateLetBlock(env, lit)
}

func (tr *Translator) translateFuncBlock(env *ExprEnv, block *parse_tree.FuncBlock) *FuncExpr {
	return tr.translateFunc(env, block.FuncBlockPieces)
}
func (tr *Translator) translateInlineFunc(env *ExprEnv, inline *parse_tree.InlineFunc) *FuncExpr {
	funcBlockPieces := []*parse_tree.FuncBlockPiece{{
		LVal: inline.LVal,
		Expr: &parse_tree.JustExprBlock{Expr: inline.Expr},
	}}
	return tr.translateFunc(env, funcBlockPieces)
}

var argPatMatBlock = &parse_tree.PatMatBlock{
	Token: token.PatMatToken,
	LVal: &parse_tree.InlineUnopExpr{
		Token: token.ShadowToken,
		Expr: &parse_tree.InlineID{
			Token: token.Token{Type: token.ID, Literal: "arg"},
			Name:  "arg",
		}},
	Expr: &parse_tree.JustExprBlock{
		Expr: &parse_tree.InlineID{Name: ArgName},
	},
}

func (tr *Translator) translateFunc(env *ExprEnv, funcBlockPieces []*parse_tree.FuncBlockPiece) *FuncExpr {
	var letExprs []*LetExpr

	for _, piece := range funcBlockPieces {
		letBlock := &parse_tree.LetBlock{
			Decls: []*parse_tree.PatMatBlock{{
				LVal: piece.LVal,
				Expr: &parse_tree.JustExprBlock{
					Expr: &parse_tree.InlineID{Name: ArgName},
				}},
				argPatMatBlock,
			},
			Expr: piece.Expr,
		}

		letExprs = append(letExprs, tr.translateLetBlock(env, letBlock))
	}

	return &FuncExpr{FuncPieceExprs: letExprs}
}

func (tr *Translator) translateTupleDestructureBlock(env *ExprEnv, block *TupleDestructureBlock) *TupleDestructureExpr {
	return &TupleDestructureExpr{
		Index: block.index,
		Tuple: tr.translateBlock(env, block.tuple),
	}
}

func (tr *Translator) translateRecordDestructureBlock(env *ExprEnv, block *RecordDestructureBlock) *RecordLookupExpr {
	return &RecordLookupExpr{
		Name:   block.name,
		Record: tr.translateBlock(env, block.record),
	}
}

func (tr *Translator) translateConsDestructureBlock(env *ExprEnv, block *ConsDestructureBlock) *ConsDestructureExpr {
	return &ConsDestructureExpr{
		IsHead: block.isHead,
		List:   tr.translateBlock(env, block.list),
	}
}

func (tr *Translator) translateInlineTuple(env *ExprEnv, inline *parse_tree.InlineTuple) *TupleExpr {
	var exprs []Expr

	for _, elem := range inline.Exprs {
		exprs = append(exprs, tr.translateInline(env, elem))
	}

	return &TupleExpr{exprs}
}
func (tr *Translator) translateTupleBlock(env *ExprEnv, block *parse_tree.TupleBlock) *TupleExpr {
	var exprs []Expr

	for _, elem := range block.Exprs {
		exprs = append(exprs, tr.translateBlock(env, elem))
	}

	return &TupleExpr{exprs}
}

func (tr *Translator) translateInlineRecord(env *ExprEnv, inline *parse_tree.InlineRecord) *RecordExpr {
	elems := map[string]Expr{}

	for k, v := range inline.Elems {
		elems[k] = tr.translateInline(env, v)
	}

	return &RecordExpr{elems, inline.PartialLVal}
}
func (tr *Translator) translateRecordBlock(env *ExprEnv, block *parse_tree.RecordBlock) *RecordExpr {
	elems := map[string]Expr{}

	for k, v := range block.Elems {
		elems[k] = tr.translateBlock(env, v)
	}

	return &RecordExpr{Elems: elems}
}

func (tr *Translator) translateInlineCons(env *ExprEnv, inline *parse_tree.InlineCons) *ConsExpr {
	if inline == parse_tree.InlineNil {
		return NilList
	}

	return &ConsExpr{
		Head: tr.translateInline(env, inline.Head),
		Tail: tr.translateInline(env, inline.Tail),
	}
}
func (tr *Translator) translateConsBlock(env *ExprEnv, block *parse_tree.ConsBlock) *ConsExpr {
	if block == parse_tree.NilBlock {
		return NilList
	}

	return &ConsExpr{
		Head: tr.translateBlock(env, block.Head),
		Tail: tr.translateBlock(env, block.Tail),
	}
}

// This is the name that we bind to the argument
// when a function is called. It's important that
// it's not a legal identifier, so it will never
// clash with identifiers in the program, and so
// user code cannot access it. (User code *can* access
// "arg", which just looks up "@arg".)
var ArgName = "@arg"

func isNil(i interface{}) bool {
	return i == nil || reflect.ValueOf(i).IsNil()
}
