package repl

import (
	"bufio"
	"crisp/ast"
	"crisp/eval"
	"crisp/lexer"
	"crisp/parser"
	"crisp/value"
	"fmt"
	"io"
)

const PROMPT = "$➤ "

func Start(in io.Reader, out io.Writer) {
	scanner := bufio.NewScanner(in)

	for {
		_, err := fmt.Fprintf(out, PROMPT)
		if err != nil {
			fmt.Print(err)
		}
		scanned := scanner.Scan()
		if !scanned {
			return
		}

		line := scanner.Text()
		l := lexer.New(line)
		p := parser.New(l)
		pTree := p.ParseProgram()

		// check for parse errors
		errors := p.Errors()
		if len(errors) > 0 {
			for _, msg := range errors {
				_, err = fmt.Fprintf(out, "   parser error: %q\n", msg)
				if err != nil {
					fmt.Print(err)
				}
			}
		}

		tr := ast.NewTranslator()
		program := tr.Translate(pTree)
		// check for translation errors
		errors = tr.Errors()
		if len(errors) > 0 {
			for _, msg := range errors {
				_, err = fmt.Fprintf(out, "   translator error: %q\n", msg)
				if err != nil {
					fmt.Print(err)
				}
			}
		}

		evaluated := eval.Eval(value.TopLevelEnv, program)
		if evaluated != nil {
			_, err = io.WriteString(out, evaluated.Inspect()+"\n")
			if err != nil {
				fmt.Print(err)
			}
		}
	}
}
