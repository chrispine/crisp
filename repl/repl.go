package repl

import (
	"bufio"
	"fmt"
	"io"
	"strconv"
	"strings"
)

func Start(in io.Reader, out io.Writer, run func(string) (string, bool)) {
	_, _ = fmt.Fprintf(out, "Welcome to the Crisp REPL!\n")
	_, _ = fmt.Fprintf(out, "Enter lines of code, then a blank line to begin evaluation.\n")
	_, _ = fmt.Fprintf(out, "(Remember to use tabs for indentation.)\n")

	scanner := bufio.NewScanner(in)

	var lines []string

	for {
		var prompt string
		if len(lines) < 1 {
			prompt = "\n$"
		} else {
			prompt = strconv.Itoa(len(lines))
		}
		prompt += "➤ "

		_, _ = fmt.Fprintf(out, prompt)
		scanned := scanner.Scan()
		if !scanned {
			return
		}

		line := scanner.Text()

		if line == "exit" {
			return
		}
		if line != "run" {
			lines = append(lines, line)
			continue
		}

		code := strings.Join(lines, "\n")
		lines = nil

		output, ok := run(code)

		if !ok {
			_, _ = fmt.Fprintf(out, "Crisp encountered an error:\n")
		}
		_, _ = fmt.Fprintf(out, output+"\n")
	}
}
