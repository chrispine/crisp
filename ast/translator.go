package ast

import (
	"crisp/parse_tree"
	"crisp/token"
	"fmt"
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

	return tr.translateBlock(lb)
}

func (tr *Translator) translateBlock(blockTree parse_tree.Block) Expr {
	switch block := blockTree.(type) {
	case *parse_tree.JustExprBlock:
		return tr.translateJustExprBlock(block)
	case *parse_tree.LetBlock:
		return tr.translateLetBlock(block)
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

func (tr *Translator) translateLetBlock(block *parse_tree.LetBlock) Expr {
	le := &LetExpr{
		Expr:     tr.translateBlock(block.Expr),
		Bindings: map[string]Expr{},
	}

	for _, someDecl := range block.Decls {
		switch decl := someDecl.(type) {
		case *parse_tree.PatMatBlock:
			switch lVal := decl.Lval.(type) {
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
