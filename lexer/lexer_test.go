package lexer

import (
	"crisp/token"
	"testing"
)

func TestVariousTokens(t *testing.T) {
	input := `main

foo = x ->
	x + 1

(3+3-2*1/1^2%999).foo

module export let case [{}]

a|日本語&c<=!d<>=> !=== ,:;`

	expectedTokens := []token.Token{
		{5, token.BlockLen, "«BlockLen»"},
		{0, token.ID, "main"},
		{0, token.NewLine, "\n"},

		{0, token.ID, "foo"},
		{0, token.PatMat, "="},
		{0, token.ID, "x"},
		{0, token.Arrow, "->"},
		{0, token.Indent, "« -> »"},
		{1, token.BlockLen, "«BlockLen»"},
		{0, token.ID, "x"},
		{0, token.Plus, "+"},
		{0, token.Int, "1"},
		{0, token.NewLine, "\n"},
		{0, token.Dedent, "« <- »"},

		{0, token.LParen, "("},
		{0, token.Int, "3"},
		{0, token.Plus, "+"},
		{0, token.Int, "3"},
		{0, token.Minus, "-"},
		{0, token.Int, "2"},
		{0, token.Mult, "*"},
		{0, token.Int, "1"},
		{0, token.Div, "/"},
		{0, token.Int, "1"},
		{0, token.Exp, "^"},
		{0, token.Int, "2"},
		{0, token.Mod, "%"},
		{0, token.Int, "999"},
		{0, token.RParen, ")"},
		{0, token.Dot, "."},
		{0, token.ID, "foo"},
		{0, token.NewLine, "\n"},

		{0, token.Module, "module"},
		{0, token.Export, "export"},
		{0, token.Let, "let"},
		{0, token.Case, "case"},
		{0, token.LBracket, "["},
		{0, token.LBrace, "{"},
		{0, token.RBrace, "}"},
		{0, token.RBracket, "]"},
		{0, token.NewLine, "\n"},

		{0, token.ID, "a"},
		{0, token.Or, "|"},
		{0, token.ID, "日本語"},
		{0, token.And, "&"},
		{0, token.ID, "c"},
		{0, token.LTE, "<="},
		{0, token.Not, "!"},
		{0, token.ID, "d"},
		{0, token.LT, "<"},
		{0, token.GTE, ">="},
		{0, token.GT, ">"},
		{0, token.NEq, "!="},
		{0, token.Equal, "=="},
		{0, token.Comma, ","},
		{0, token.Colon, ":"},
		{0, token.Semicolon, ";"},
		{0, token.NewLine, "\n"},
		{0, token.EOF, "«EOF»"},
	}

	testInput(t, input, expectedTokens)
}

func TestTokenizingBlocks1(t *testing.T) {
	input := `(*)
	1
	2`

	expectedTokens := []token.Token{
		{1, token.BlockLen, "«BlockLen»"},
		{0, token.TBlock, "(*)"},
		{0, token.Indent, "« -> »"},
		{2, token.BlockLen, "«BlockLen»"},
		{0, token.Int, "1"},
		{0, token.NewLine, "\n"},
		{0, token.Int, "2"},
		{0, token.NewLine, "\n"},
		{0, token.Dedent, "« <- »"},
		{0, token.EOF, "«EOF»"},
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
		{1, token.BlockLen, "«BlockLen»"},
		{0, token.TBlock, "(*)"},
		{0, token.Indent, "« -> »"},
		{2, token.BlockLen, "«BlockLen»"},
		{0, token.Int, "1"},
		{0, token.NewLine, "\n"},
		{0, token.Int, "2"},
		{0, token.NewLine, "\n"},
		{0, token.Dedent, "« <- »"},
		{0, token.EOF, "«EOF»"},
	}

	testInput(t, input, expectedTokens)
}

func TestTokenizingBlocks3(t *testing.T) {
	input := `(*)
	1
x
`

	expectedTokens := []token.Token{
		{2, token.BlockLen, "«BlockLen»"},
		{0, token.TBlock, "(*)"},
		{0, token.Indent, "« -> »"},
		{1, token.BlockLen, "«BlockLen»"},
		{0, token.Int, "1"},
		{0, token.NewLine, "\n"},
		{0, token.Dedent, "« <- »"},

		{0, token.ID, "x"},
		{0, token.NewLine, "\n"},
		{0, token.EOF, "«EOF»"},
	}

	testInput(t, input, expectedTokens)
}

func TestTokenizingBlocks4(t *testing.T) {
	input := `# comment
(*)
	1
x`

	expectedTokens := []token.Token{
		{2, token.BlockLen, "«BlockLen»"},
		{0, token.TBlock, "(*)"},
		{0, token.Indent, "« -> »"},
		{1, token.BlockLen, "«BlockLen»"},
		{0, token.Int, "1"},
		{0, token.NewLine, "\n"},
		{0, token.Dedent, "« <- »"},

		{0, token.ID, "x"},
		{0, token.NewLine, "\n"},
		{0, token.EOF, "«EOF»"},
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
		{1, token.BlockLen, "«BlockLen»"},
		{0, token.TBlock, "(*)"},
		{0, token.Indent, "« -> »"},
		{2, token.BlockLen, "«BlockLen»"},
		{0, token.Int, "1"},
		{0, token.NewLine, "\n"},
		{0, token.TBlock, "(*)"},
		{0, token.Indent, "« -> »"},
		{1, token.BlockLen, "«BlockLen»"},
		{0, token.Int, "2"},
		{0, token.NewLine, "\n"},
		{0, token.Dedent, "« <- »"},
		{0, token.Dedent, "« <- »"},
		{0, token.EOF, "«EOF»"},
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
		{1, token.BlockLen, "«BlockLen»"},
		{0, token.TBlock, "(*)"},
		{0, token.Indent, "« -> »"},
		{2, token.BlockLen, "«BlockLen»"},
		{0, token.Int, "1"},
		{0, token.NewLine, "\n"},
		{0, token.TBlock, "(*)"},
		{0, token.Indent, "« -> »"},
		{1, token.BlockLen, "«BlockLen»"},
		{0, token.Int, "2"},
		{0, token.NewLine, "\n"},
		{0, token.Dedent, "« <- »"},
		{0, token.Dedent, "« <- »"},
		{0, token.EOF, "«EOF»"},
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
		{2, token.BlockLen, "«BlockLen»"},
		{0, token.TBlock, "(*)"},
		{0, token.Indent, "« -> »"},
		{2, token.BlockLen, "«BlockLen»"},
		{0, token.Int, "1"},
		{0, token.NewLine, "\n"},
		{0, token.TBlock, "(*)"},
		{0, token.Indent, "« -> »"},
		{1, token.BlockLen, "«BlockLen»"},
		{0, token.Int, "2"},
		{0, token.NewLine, "\n"},
		{0, token.Dedent, "« <- »"},
		{0, token.Dedent, "« <- »"},

		{0, token.ID, "x"},
		{0, token.NewLine, "\n"},
		{0, token.EOF, "«EOF»"},
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
