package lexer

import (
	"crisp/token"
	"testing"
)

func TestVariousTokens(t *testing.T) {
	input := `main

fooBar = x ->
	x + 1

(3+3-2*1/1.44^2%999).foo_bar

module export let case [{}]

a|日本語&c<=!d<>=> !=== ,:;`

	expectedTokens := []token.Token{
		{NumLines: 5, Type: token.BlockLen,  Literal: "«BlockLen»"},
		{NumLines: 0, Type: token.ID,        Literal: "main"},
		{NumLines: 0, Type: token.NewLine,   Literal: "\n"},

		{NumLines: 0, Type: token.ID,        Literal: "fooBar"},
		{NumLines: 0, Type: token.PatMat,    Literal: "="},
		{NumLines: 0, Type: token.ID,        Literal: "x"},
		{NumLines: 0, Type: token.Arrow,     Literal: "->"},
		{NumLines: 0, Type: token.Indent,    Literal: "« -> »"},
		{NumLines: 1, Type: token.BlockLen,  Literal: "«BlockLen»"},
		{NumLines: 0, Type: token.ID,        Literal: "x"},
		{NumLines: 0, Type: token.Plus,      Literal: "+"},
		{NumLines: 0, Type: token.ID,        Literal: "1"},
		{NumLines: 0, Type: token.NewLine,   Literal: "\n"},
		{NumLines: 0, Type: token.Dedent,    Literal: "« <- »"},

		{NumLines: 0, Type: token.LParen,    Literal: "("},
		{NumLines: 0, Type: token.ID,        Literal: "3"},
		{NumLines: 0, Type: token.Plus,      Literal: "+"},
		{NumLines: 0, Type: token.ID,        Literal: "3"},
		{NumLines: 0, Type: token.Minus,     Literal: "-"},
		{NumLines: 0, Type: token.ID,        Literal: "2"},
		{NumLines: 0, Type: token.Mult,      Literal: "*"},
		{NumLines: 0, Type: token.ID,        Literal: "1"},
		{NumLines: 0, Type: token.Div,       Literal: "/"},
		{NumLines: 0, Type: token.Float,     Literal: "1.44"},
		{NumLines: 0, Type: token.Exp,       Literal: "^"},
		{NumLines: 0, Type: token.ID,        Literal: "2"},
		{NumLines: 0, Type: token.Mod,       Literal: "%"},
		{NumLines: 0, Type: token.ID,        Literal: "999"},
		{NumLines: 0, Type: token.RParen,    Literal: ")"},
		{NumLines: 0, Type: token.Dot,       Literal: "."},
		{NumLines: 0, Type: token.ID,        Literal: "foo_bar"},
		{NumLines: 0, Type: token.NewLine,   Literal: "\n"},

		{NumLines: 0, Type: token.Module,    Literal: "module"},
		{NumLines: 0, Type: token.Export,    Literal: "export"},
		{NumLines: 0, Type: token.Let,       Literal: "let"},
		{NumLines: 0, Type: token.Case,      Literal: "case"},
		{NumLines: 0, Type: token.LBracket,  Literal: "["},
		{NumLines: 0, Type: token.LBrace,    Literal: "{"},
		{NumLines: 0, Type: token.RBrace,    Literal: "}"},
		{NumLines: 0, Type: token.RBracket,  Literal: "]"},
		{NumLines: 0, Type: token.NewLine,   Literal: "\n"},

		{NumLines: 0, Type: token.ID,        Literal: "a"},
		{NumLines: 0, Type: token.Or,        Literal: "|"},
		{NumLines: 0, Type: token.ID,        Literal: "日本語"},
		{NumLines: 0, Type: token.And,       Literal: "&"},
		{NumLines: 0, Type: token.ID,        Literal: "c"},
		{NumLines: 0, Type: token.LTE,       Literal: "<="},
		{NumLines: 0, Type: token.ID,        Literal: "!"},
		{NumLines: 0, Type: token.ID,        Literal: "d"},
		{NumLines: 0, Type: token.LT,        Literal: "<"},
		{NumLines: 0, Type: token.GTE,       Literal: ">="},
		{NumLines: 0, Type: token.GT,        Literal: ">"},
		{NumLines: 0, Type: token.NEq,       Literal: "!="},
		{NumLines: 0, Type: token.Equal,     Literal: "=="},
		{NumLines: 0, Type: token.Comma,     Literal: ","},
		{NumLines: 0, Type: token.Colon,     Literal: ":"},
		{NumLines: 0, Type: token.Semicolon, Literal: ";"},
		{NumLines: 0, Type: token.NewLine,   Literal: "\n"},
		{NumLines: 0, Type: token.EOF,       Literal: "«EOF»"},
	}

	testInput(t, input, expectedTokens)
}

func TestTokenizingWhitespaceAtStart(t *testing.T) {
	input := `
5`

	expectedTokens := []token.Token{
		{NumLines: 1, Type: token.BlockLen, Literal: "«BlockLen»"},
		{NumLines: 0, Type: token.ID,       Literal: "5"},
		{NumLines: 0, Type: token.NewLine,  Literal: "\n"},
		{NumLines: 0, Type: token.EOF,      Literal: "«EOF»"},
	}

	testInput(t, input, expectedTokens)
}

func TestTokenizingBlocks1(t *testing.T) {
	input := `(*)
	1.2
	2`

	expectedTokens := []token.Token{
		{NumLines: 1, Type: token.BlockLen, Literal: "«BlockLen»"},
		{NumLines: 0, Type: token.TBlock,   Literal: "(*)"},
		{NumLines: 0, Type: token.Indent,   Literal: "« -> »"},
		{NumLines: 2, Type: token.BlockLen, Literal: "«BlockLen»"},
		{NumLines: 0, Type: token.Float,    Literal: "1.2"},
		{NumLines: 0, Type: token.NewLine,  Literal: "\n"},
		{NumLines: 0, Type: token.ID,       Literal: "2"},
		{NumLines: 0, Type: token.NewLine,  Literal: "\n"},
		{NumLines: 0, Type: token.Dedent,   Literal: "« <- »"},
		{NumLines: 0, Type: token.EOF,      Literal: "«EOF»"},
	}

	testInput(t, input, expectedTokens)
}

func TestTokenizingBlocks2(t *testing.T) {
	input := `# comment
(*)
	1
	2
`

	expectedTokens := []token.Token{
		{NumLines: 1, Type: token.BlockLen, Literal: "«BlockLen»"},
		{NumLines: 0, Type: token.TBlock,   Literal: "(*)"},
		{NumLines: 0, Type: token.Indent,   Literal: "« -> »"},
		{NumLines: 2, Type: token.BlockLen, Literal: "«BlockLen»"},
		{NumLines: 0, Type: token.ID,       Literal: "1"},
		{NumLines: 0, Type: token.NewLine,  Literal: "\n"},
		{NumLines: 0, Type: token.ID,       Literal: "2"},
		{NumLines: 0, Type: token.NewLine,  Literal: "\n"},
		{NumLines: 0, Type: token.Dedent,   Literal: "« <- »"},
		{NumLines: 0, Type: token.EOF,      Literal: "«EOF»"},
	}

	testInput(t, input, expectedTokens)
}

func TestTokenizingBlocks3(t *testing.T) {
	input := `(*)
	1
x
`

	expectedTokens := []token.Token{
		{NumLines: 2, Type: token.BlockLen, Literal: "«BlockLen»"},
		{NumLines: 0, Type: token.TBlock,   Literal: "(*)"},
		{NumLines: 0, Type: token.Indent,   Literal: "« -> »"},
		{NumLines: 1, Type: token.BlockLen, Literal: "«BlockLen»"},
		{NumLines: 0, Type: token.ID,       Literal: "1"},
		{NumLines: 0, Type: token.NewLine,  Literal: "\n"},
		{NumLines: 0, Type: token.Dedent,   Literal: "« <- »"},

		{NumLines: 0, Type: token.ID,       Literal: "x"},
		{NumLines: 0, Type: token.NewLine,  Literal: "\n"},
		{NumLines: 0, Type: token.EOF,      Literal: "«EOF»"},
	}

	testInput(t, input, expectedTokens)
}

func TestTokenizingBlocks4(t *testing.T) {
	input := `# comment
(*)
	1
x`

	expectedTokens := []token.Token{
		{NumLines: 2, Type: token.BlockLen, Literal: "«BlockLen»"},
		{NumLines: 0, Type: token.TBlock,   Literal: "(*)"},
		{NumLines: 0, Type: token.Indent,   Literal: "« -> »"},
		{NumLines: 1, Type: token.BlockLen, Literal: "«BlockLen»"},
		{NumLines: 0, Type: token.ID,       Literal: "1"},
		{NumLines: 0, Type: token.NewLine,  Literal: "\n"},
		{NumLines: 0, Type: token.Dedent,   Literal: "« <- »"},

		{NumLines: 0, Type: token.ID,       Literal: "x"},
		{NumLines: 0, Type: token.NewLine,  Literal: "\n"},
		{NumLines: 0, Type: token.EOF,      Literal: "«EOF»"},
	}

	testInput(t, input, expectedTokens)
}

func TestTokenizingBlocks5(t *testing.T) {
	input := `# comment
(*)
	1
	(*)
		2`

	expectedTokens := []token.Token{
		{NumLines: 1, Type: token.BlockLen, Literal: "«BlockLen»"},
		{NumLines: 0, Type: token.TBlock,   Literal: "(*)"},
		{NumLines: 0, Type: token.Indent,   Literal: "« -> »"},
		{NumLines: 2, Type: token.BlockLen, Literal: "«BlockLen»"},
		{NumLines: 0, Type: token.ID,       Literal: "1"},
		{NumLines: 0, Type: token.NewLine,  Literal: "\n"},
		{NumLines: 0, Type: token.TBlock,   Literal: "(*)"},
		{NumLines: 0, Type: token.Indent,   Literal: "« -> »"},
		{NumLines: 1, Type: token.BlockLen, Literal: "«BlockLen»"},
		{NumLines: 0, Type: token.ID,       Literal: "2"},
		{NumLines: 0, Type: token.NewLine,  Literal: "\n"},
		{NumLines: 0, Type: token.Dedent,   Literal: "« <- »"},
		{NumLines: 0, Type: token.Dedent,   Literal: "« <- »"},
		{NumLines: 0, Type: token.EOF,      Literal: "«EOF»"},
	}

	testInput(t, input, expectedTokens)
}

func TestTokenizingBlocks6(t *testing.T) {
	input := `# comment
(*)
	1
	(*)
		2
`

	expectedTokens := []token.Token{
		{NumLines: 1, Type: token.BlockLen, Literal: "«BlockLen»"},
		{NumLines: 0, Type: token.TBlock,   Literal: "(*)"},
		{NumLines: 0, Type: token.Indent,   Literal: "« -> »"},
		{NumLines: 2, Type: token.BlockLen, Literal: "«BlockLen»"},
		{NumLines: 0, Type: token.ID,       Literal: "1"},
		{NumLines: 0, Type: token.NewLine,  Literal: "\n"},
		{NumLines: 0, Type: token.TBlock,   Literal: "(*)"},
		{NumLines: 0, Type: token.Indent,   Literal: "« -> »"},
		{NumLines: 1, Type: token.BlockLen, Literal: "«BlockLen»"},
		{NumLines: 0, Type: token.ID,       Literal: "2"},
		{NumLines: 0, Type: token.NewLine,  Literal: "\n"},
		{NumLines: 0, Type: token.Dedent,   Literal: "« <- »"},
		{NumLines: 0, Type: token.Dedent,   Literal: "« <- »"},
		{NumLines: 0, Type: token.EOF,      Literal: "«EOF»"},
	}

	testInput(t, input, expectedTokens)
}

func TestTokenizingBlocks7(t *testing.T) {
	input := `# comment
(*)
	1
	(*)
		2

x`

	expectedTokens := []token.Token{
		{NumLines: 2, Type: token.BlockLen, Literal: "«BlockLen»"},
		{NumLines: 0, Type: token.TBlock,   Literal: "(*)"},
		{NumLines: 0, Type: token.Indent,   Literal: "« -> »"},
		{NumLines: 2, Type: token.BlockLen, Literal: "«BlockLen»"},
		{NumLines: 0, Type: token.ID,       Literal: "1"},
		{NumLines: 0, Type: token.NewLine,  Literal: "\n"},
		{NumLines: 0, Type: token.TBlock,   Literal: "(*)"},
		{NumLines: 0, Type: token.Indent,   Literal: "« -> »"},
		{NumLines: 1, Type: token.BlockLen, Literal: "«BlockLen»"},
		{NumLines: 0, Type: token.ID,       Literal: "2"},
		{NumLines: 0, Type: token.NewLine,  Literal: "\n"},
		{NumLines: 0, Type: token.Dedent,   Literal: "« <- »"},
		{NumLines: 0, Type: token.Dedent,   Literal: "« <- »"},

		{NumLines: 0, Type: token.ID,       Literal: "x"},
		{NumLines: 0, Type: token.NewLine,  Literal: "\n"},
		{NumLines: 0, Type: token.EOF,      Literal: "«EOF»"},
	}

	testInput(t, input, expectedTokens)
}

func testInput(t *testing.T, input string, expectedTokens []token.Token) {
	l := New(input)

	var numNewlines, numIndents, numDedents, sumBlockLens int

	for i, tt := range expectedTokens {
		tok := l.NextToken()

		switch tok.Type {
		case token.NewLine:
			numNewlines++
		case token.Indent:
			numIndents++
		case token.Dedent:
			numDedents++
		case token.BlockLen:
			sumBlockLens += tok.NumLines
		}

		if tok.Literal != tt.Literal {
			t.Fatalf("tests[%d] - literal wrong. expected=%q, got=%q",
				i, tt.Literal, tok.Literal)
		}

		if tok.NumLines != tt.NumLines {
			t.Fatalf("tests[%d] - numLines wrong. expected=%v, got=%v",
				i, tt.NumLines, tok.NumLines)
		}

		if tok.Type != tt.Type {
			t.Fatalf("tests[%d] - tokentype wrong. expected=%q, got=%q",
				i, tt.Type, tok.Type)
		}
	}

	if numIndents != numDedents {
		t.Fatalf("indents(%v) != dedents(%v)", numIndents, numDedents)
	}

	if numNewlines+numIndents != sumBlockLens {
		t.Fatalf("newlines(%v)+indents(%v) != blockLens(%v)", numNewlines, numIndents, sumBlockLens)
	}
}
