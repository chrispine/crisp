package main

import (
	"crisp/ast"
	"crisp/eval"
	"crisp/lexer"
	"crisp/parser"
	"crisp/repl"
	"fmt"
	"io/ioutil"
	"os"
)

func main() {
	if len(os.Args) < 2 {
		// no filename passed in, so start the REPL
		repl.Start(os.Stdin, os.Stdout, run)
		return
	}

	// run the given program
	filename := os.Args[1]
	file, err := ioutil.ReadFile(filename)
	if err != nil {
		fmt.Print("file not found: " + filename)
		return
	}

	output, ok := run(string(file))

	if !ok {
		fmt.Println("Crisp encountered an error:")
	}
	fmt.Print(output)
}

func run(code string) (string, bool) {
	l := lexer.New(code)
	p := parser.New(l)
	pTree, err := p.ParseProgram()
	if err != nil {
		return err.Error(), false
	}

	tr := ast.NewTranslator(eval.CreateNativeFuncs())
	program := tr.Translate(pTree)

	// check for translation errors
	errStr := ""
	errors := tr.Errors()
	if len(errors) > 0 {
		for _, msg := range errors {
			errStr += fmt.Sprintf("   translator error: %q\n", msg)
		}
		return errStr, false
	}

	val, err := eval.Eval(eval.TopLevelEnv, program)
	if err != nil {
		errStr = fmt.Sprintf("   runtime error: %q\n", err)
		return errStr, false
	}

	output := val.Inspect()

	//tipe := program.FinalTipe()
	//tipeStr, _ := tipe.TipeString('A')
	//output += "\nType: " + tipeStr

	return output, true
}
