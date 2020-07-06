package ast

import (
	"crisp/parse_tree"
	"crisp/token"
	"fmt"
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

	return tr.translateBlock(TopLevelExprEnv, lb)
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
	}

	tr.error(fmt.Sprintf("Translator Error: unhandled Inline %v of type %T", inlineTree, inlineTree))
	return nil
}

func (tr *Translator) translateJustExprBlock(env *ExprEnv, block *parse_tree.JustExprBlock) Expr {
	return tr.translateInline(env, block.Expr)
}

func (tr *Translator) translateLetBlock(env *ExprEnv, block *parse_tree.LetBlock) *LetExpr {
	newEnv := &ExprEnv{
		parent: env,
	}

	le := &LetExpr{Env: newEnv}

	type toAssert struct {
		lVal parse_tree.Inline
		expr parse_tree.Block
	}

	// For each declaration (LVal = Expr), we break it down
	// into new bindings (which go into newEnv.Bindings)
	// and new assertions (which go into le.Asserts).
	// Note that we cannot translate the sub-expressions until
	// the env is complete, so we save them up and do that last.
	var bindingsToTranslate []parse_tree.Block
	var assertsToTranslate []*toAssert

	for _, decl := range block.Decls {
		switch lVal := decl.LVal.(type) {
		case *parse_tree.InlineID:
			if env.isDefined(lVal.Name) {
				// assert
				assertsToTranslate = append(assertsToTranslate, &toAssert{decl.LVal, decl.Expr})
			} else {
				// binding
				newEnv.Bindings = append(newEnv.Bindings, ExprBinding{lVal.Name, nil})
				bindingsToTranslate = append(bindingsToTranslate, decl.Expr)
			}
		default:
			tr.error("someone tell Chris to implement destructuring assignment")
		}
	}

	// Now that we know the name half of all bindings in newEnv,
	// we can proceed with translation of sub-expressions.
	for _, ta := range assertsToTranslate {
		newAssert := &BinopExpr{
			Token: token.Token{Type: token.Equal, Literal: "=="},
			LExpr: tr.translateInline(newEnv, ta.lVal),
			RExpr: tr.translateBlock(newEnv, ta.expr),
		}
		le.Asserts = append(le.Asserts, newAssert)
	}
	for i, bindBlock := range bindingsToTranslate {
		newEnv.Bindings[i].Expr = tr.translateBlock(newEnv, bindBlock)
	}

	le.Expr = tr.translateBlock(newEnv, block.Expr)

	return le
}

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
		parent:   env,
		Bindings: []ExprBinding{{Name: argName, Expr: Arg}},
	}

	letExpr := tr.translateLetBlock(argEnv, letBlock)

	return &FuncExpr{FuncPartExprs: []*LetExpr{letExpr}, ArgName: argName}
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
