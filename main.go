package main

import (
	"crisp/repl"
	"fmt"
	"os"
)

func main() {
	fmt.Println("Welcome to the Crisp REPL.")
	fmt.Println("Gimme some code!")

	repl.Start(os.Stdin, os.Stdout)
}
