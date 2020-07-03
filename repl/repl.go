package repl

import (
	"bufio"
	"crisp/ast"
	"crisp/lexer"
	"crisp/parser"
	"fmt"
	"io"
)

const PROMPT = "$➤ "

func Start(in io.Reader, out io.Writer) {
	scanner := bufio.NewScanner(in)

	for {
		fmt.Fprintf(out, PROMPT)
		scanned := scanner.Scan()
		if !scanned {
			return
		}

		line := scanner.Text()
		l := lexer.New(line)
		p := parser.New(l)
		exprAST := p.ParseProgram().Expr.(*ast.JustExprBlock).Expr

		// check for parse errors
		errors := p.Errors()
		if len(errors) > 0 {
			for _, msg := range errors {
				fmt.Fprintf(out, "   parser error: %q\n", msg)
			}
		}

		fmt.Fprintf(out, "   %+v\n", exprAST)
	}
}
