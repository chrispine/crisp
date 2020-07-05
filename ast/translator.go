package ast

import (
	"crisp/parse_tree"
	"fmt"
)

// translates parse trees into ASTs

type Translator struct {
	errors []string
}

func NewTranslator() *Translator { return &Translator{} }

// A Crisp program is treated as if it is the interior of a `let` expression.
func (tr *Translator) Translate(program *parse_tree.Program) Expr {

	le := &LetExpr{
		Expr: tr.convertBlock(program.Expr),
	}

	for _, decl := range program.Decls {
		decl.String() // TODO: actually do something
	}

	return le
}

func (tr *Translator) convertBlock(blockTree parse_tree.Block) Expr {
	switch block := blockTree.(type) {
	case *parse_tree.JustExprBlock:
		return tr.convertInline(block.Expr)
	}

	tr.error(fmt.Sprintf("Translator Error: unhandled Block %v of type %T", blockTree, blockTree))
	return nil
}

func (tr *Translator) convertInline(inlineTree parse_tree.Inline) Expr {
	switch inline := inlineTree.(type) {
	case *parse_tree.InlineInt:
		return &IntExpr{Value: inline.Value}
	case *parse_tree.InlineUnopExpr:
		return &UnopExpr{
			Token: inline.Token,
			Expr:  tr.convertInline(inline.Expr),
		}
	case *parse_tree.InlineBinopExpr:
		return &BinopExpr{
			Token: inline.Token,
			LExpr: tr.convertInline(inline.LExpr),
			RExpr: tr.convertInline(inline.RExpr),
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
