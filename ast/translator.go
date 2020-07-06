package ast

import (
	"crisp/parse_tree"
	"crisp/token"
	"fmt"
)

// This is the name that we bind to the argument
// when a function is called. It's important that
// it's not a legal identifier, so it will never
// clash with identifiers in the program.
const ArgName = "@ARG@"

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

	return tr.translateBlock(lb)
}

func (tr *Translator) translateBlock(blockTree parse_tree.Block) Expr {
	switch block := blockTree.(type) {
	case *parse_tree.JustExprBlock:
		return tr.translateJustExprBlock(block)
	case *parse_tree.LetBlock:
		return tr.translateLetBlock(block)
	case *parse_tree.FuncBlock:
		return tr.translateFuncBlock(block)
	}

	tr.error(fmt.Sprintf("Translator Error: unhandled Block %v of type %T", blockTree, blockTree))
	return nil
}

func (tr *Translator) translateInline(inlineTree parse_tree.Inline) Expr {
	switch inline := inlineTree.(type) {
	case *parse_tree.InlineInt:
		return &IntExpr{Value: inline.Value}
	case *parse_tree.InlineBool:
		return &BoolExpr{Value: inline.Value}
	case *parse_tree.InlineID:
		return &LookupExpr{Name: inline.Name}
	case *parse_tree.InlineUnopExpr:
		return &UnopExpr{
			Token: inline.Token,
			Expr:  tr.translateInline(inline.Expr),
		}
	case *parse_tree.InlineBinopExpr:
		return &BinopExpr{
			Token: inline.Token,
			LExpr: tr.translateInline(inline.LExpr),
			RExpr: tr.translateInline(inline.RExpr),
		}
	case *parse_tree.InlineFunc:
		return tr.translateInlineFunc(inline)
	}

	tr.error(fmt.Sprintf("Translator Error: unhandled Inline %v of type %T", inlineTree, inlineTree))
	return nil
}

func (tr *Translator) Errors() []string {
	return tr.errors
}

func (tr *Translator) error(err string) {
	tr.errors = append(tr.errors, err)
	panic(err) // TODO: remove this line when parser is stable
}

func (tr *Translator) translateJustExprBlock(block *parse_tree.JustExprBlock) Expr {
	return tr.translateInline(block.Expr)
}

func (tr *Translator) translateLetBlock(block *parse_tree.LetBlock) *LetExpr {
	le := &LetExpr{
		Expr:     tr.translateBlock(block.Expr),
		Bindings: map[string]Expr{},
	}

	for _, someDecl := range block.Decls {
		switch decl := someDecl.(type) {
		case *parse_tree.PatMatBlock:
			switch lVal := decl.LVal.(type) {
			case *parse_tree.InlineID:
				le.Bindings[lVal.Name] = tr.translateBlock(decl.Expr)
			default:
				tr.error("someone tell Chris to implement destructuring assignment")
			}
		default:
			tr.error(fmt.Sprintf("Translator Error: unhandled declaration %v of type %T", decl, decl))
		}
	}

	return le
}

func (tr *Translator) translateFuncBlock(block *parse_tree.FuncBlock) Expr {
	letBlock := &parse_tree.LetBlock{
		Decls: []parse_tree.Block{
			&parse_tree.PatMatBlock{
				LVal: block.LVal,
				Expr: &parse_tree.JustExprBlock{
					Expr: &parse_tree.InlineID{Name: ArgName},
				},
			},
		},
		Expr: block.Expr,
	}

	letExpr := tr.translateLetBlock(letBlock)

	return tr.translateFunc(letExpr)
}
func (tr *Translator) translateInlineFunc(inline *parse_tree.InlineFunc) Expr {
	letBlock := &parse_tree.LetBlock{
		Decls: []parse_tree.Block{
			&parse_tree.PatMatBlock{
				LVal: inline.LVal,
				Expr: &parse_tree.JustExprBlock{
					Expr: &parse_tree.InlineID{Name: ArgName},
				},
			},
		},
		Expr: &parse_tree.JustExprBlock{
			Expr: inline.Expr,
		},
	}

	letExpr := tr.translateLetBlock(letBlock)

	return tr.translateFunc(letExpr)
}
func (tr *Translator) translateFunc(letExpr *LetExpr) Expr {
	return &FuncExpr{FuncPartExprs: []*LetExpr{letExpr}}
}
