package ast

import (
	"crisp/parse_tree"
	"crisp/token"
	"fmt"
	"reflect"
	"strconv"
)

// translates parse trees into ASTs

type Translator struct {
	errors []string
}

func NewTranslator() *Translator { return &Translator{} }

// A Crisp program is treated as if it is the interior of a `let` expression.
func (tr *Translator) Translate(program *parse_tree.Program) Expr {
	lb := &parse_tree.LetBlock{
		Token: token.Token{Type: token.Let, Literal: "let"},
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
	case *parse_tree.TupleBlock:
		return tr.translateTupleBlock(env, block)
	case *parse_tree.ConsBlock:
		return tr.translateConsBlock(env, block)
	case *TupleDestructureBlock:
		return tr.translateTupleDestructureBlock(env, block)
	case *ConsDestructureBlock:
		return tr.translateConsDestructureBlock(env, block)
	}

	tr.error(fmt.Sprintf("Translator Error: unhandled Block %v of type %T", blockTree, blockTree))
	return nil
}

func (tr *Translator) translateInline(env *ExprEnv, inlineTree parse_tree.Inline) Expr {
	switch inline := inlineTree.(type) {
	case *parse_tree.InlineID:
		return &LookupExpr{Name: inline.Name} // TODO optimize lookup
	case *parse_tree.InlineUnopExpr:
		return &UnopExpr{
			Token: inline.Token,
			Expr:  tr.translateInline(env, inline.Expr),
		}
	case *parse_tree.InlineBinopExpr:
		return &BinopExpr{
			Token: inline.Token,
			LExpr: tr.translateInline(env, inline.LExpr),
			RExpr: tr.translateInline(env, inline.RExpr),
		}
	case *parse_tree.InlineFunc:
		return tr.translateInlineFunc(env, inline)
	case *parse_tree.InlineTuple:
		return tr.translateInlineTuple(env, inline)
	case *parse_tree.InlineCons:
		return tr.translateInlineCons(env, inline)
	}

	tr.error(fmt.Sprintf("Translator Error: unhandled Inline %v of type %T", inlineTree, inlineTree))
	return nil
}

func (tr *Translator) translateJustExprBlock(env *ExprEnv, block *parse_tree.JustExprBlock) Expr {
	return tr.translateInline(env, block.Expr)
}

type toAssert struct {
	lhsEqual   parse_tree.Inline
	rhsEqual   parse_tree.Block
	listIsCons parse_tree.Block
	listIsNil  parse_tree.Block
}

var equalToken = token.Token{Type: token.Equal, Literal: "=="}

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
		preAsserts = tr.partitionDecl(preEnv, postEnv, preAsserts, decl.LVal, decl.Expr)
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
			newAssert = &BinopExpr{
				Token: equalToken,
				LExpr: tr.translateInline(postEnv, ta.lhsEqual),
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
	rhs parse_tree.Block) []toAssert {

	switch lhs := lVal.(type) {
	case *parse_tree.InlineID:
		// `x = expr`
		if postEnv.isDefined(lhs.Name) {
			// assert
			asserts = append(asserts, toAssert{lhsEqual: lhs, rhsEqual: rhs})
		} else {
			// binding
			preEnv.Bindings = append(preEnv.Bindings, ParseBinding{Name: lhs.Name, Expr: rhs})
			postEnv.Bindings = append(postEnv.Bindings, ExprBinding{Name: lhs.Name, Expr: nil})
			// that `nil` will be populated in just a bit with the translated `rhs`
		}
	case *parse_tree.InlineTuple:
		// `(a, b) = expr`
		// Note that we don't need to assert that `rhs` is a tuple, or what size of tuple,
		// because the type checker will flag that at compile time.
		for i, elem := range lhs.Exprs {
			td := &TupleDestructureBlock{i, rhs}
			asserts = tr.partitionDecl(preEnv, postEnv, asserts, elem, td)
		}
	case *parse_tree.InlineCons:
		// `[a, b]` = expr
		// `[h; t]` = expr
		// `[    ]` = expr  (requires nil check!)
		asserts = append(asserts, toAssert{listIsCons: rhs})

		rhsHead := &ConsDestructureBlock{true, rhs}
		asserts = tr.partitionDecl(preEnv, postEnv, asserts, lhs.Head, rhsHead)

		rhsTail := &ConsDestructureBlock{false, rhs}
		if isNil(lhs.Tail) {
			asserts = append(asserts, toAssert{listIsNil: rhsTail})
		} else {
			asserts = tr.partitionDecl(preEnv, postEnv, asserts, lhs.Tail, rhsTail)
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

func (tr *Translator) translateFuncBlock(env *ExprEnv, block *parse_tree.FuncBlock) Expr {
	return tr.translateFunc(env, block.LVal, block.Expr)
}
func (tr *Translator) translateInlineFunc(env *ExprEnv, inline *parse_tree.InlineFunc) Expr {
	return tr.translateFunc(env, inline.LVal, &parse_tree.JustExprBlock{Expr: inline.Expr})
}
func (tr *Translator) translateFunc(env *ExprEnv, lVal parse_tree.Inline, expr parse_tree.Block) Expr {
	argName := getArgName()

	letBlock := &parse_tree.LetBlock{
		Decls: []*parse_tree.PatMatBlock{{
			LVal: lVal,
			Expr: &parse_tree.JustExprBlock{
				Expr: &parse_tree.InlineID{Name: argName},
			}},
		},
		Expr: expr,
	}

	argEnv := &ExprEnv{
		Parent:   env,
		Bindings: []ExprBinding{{Name: argName, Expr: Arg}},
	}

	letExpr := tr.translateLetBlock(argEnv, letBlock)

	return &FuncExpr{FuncPartExprs: []*LetExpr{letExpr}, ArgName: argName}
}

func (tr *Translator) translateTupleDestructureBlock(env *ExprEnv, block *TupleDestructureBlock) Expr {
	return &TupleDestructureExpr{
		Index: block.index,
		Tuple: tr.translateBlock(env, block.tuple),
	}
}

func (tr *Translator) translateConsDestructureBlock(env *ExprEnv, block *ConsDestructureBlock) Expr {
	return &ConsDestructureExpr{
		IsHead: block.isHead,
		List:   tr.translateBlock(env, block.list),
	}
}

func (tr *Translator) translateInlineTuple(env *ExprEnv, inline *parse_tree.InlineTuple) Expr {
	var exprs []Expr

	for _, elem := range inline.Exprs {
		exprs = append(exprs, tr.translateInline(env, elem))
	}

	return &TupleExpr{exprs}
}
func (tr *Translator) translateTupleBlock(env *ExprEnv, block *parse_tree.TupleBlock) Expr {
	var exprs []Expr

	for _, elem := range block.Exprs {
		exprs = append(exprs, tr.translateBlock(env, elem))
	}

	return &TupleExpr{exprs}
}

func (tr *Translator) translateInlineCons(env *ExprEnv, inline *parse_tree.InlineCons) Expr {
	var tail Expr

	if !isNil(inline.Tail) {
		tail = tr.translateInline(env, inline.Tail)
	}

	return &ConsExpr{
		Head: tr.translateInline(env, inline.Head),
		Tail: tail,
	}
}
func (tr *Translator) translateConsBlock(env *ExprEnv, block *parse_tree.ConsBlock) Expr {
	var tail Expr

	if !isNil(block.Tail) {
		tail = tr.translateBlock(env, block.Tail)
	}

	return &ConsExpr{
		Head: tr.translateBlock(env, block.Head),
		Tail: tail,
	}
}

// This is the name that we bind to the argument
// when a function is called. It's important that
// it's not a legal identifier, so it will never
// clash with identifiers in the program, and that
// it be unique, so it doesn't clash with nested
// function calls.
var argNum = 0

func getArgName() string {
	name := "@ARG_" + strconv.Itoa(argNum) + "@"
	argNum++

	return name
}

func isNil(i interface{}) bool {
	return i == nil || reflect.ValueOf(i).IsNil()
}
