package lexer

import (
	"crisp/token"
	"testing"
)

func TestNextToken(t *testing.T) {
	input := `# test program
foo = x ->
	x + 1

(3+3-2*1/1^2%999).foo

module export let case [{}]

a|日本語&c<=!d<>=> !=== ,:;`

	tests := []struct {
		expectedNumLines int
		expectedType     token.TokenType
		expectedLiteral  string
	}{
		{4, token.BLOCK_LEN, "«BLOCK_LEN»"},
		{0, token.ID, "foo"},
		{0, token.PATMAT, "="},
		{0, token.ID, "x"},
		{0, token.ARROW, "->"},
		{0, token.INDENT, "« -> »"},
		{1, token.BLOCK_LEN, "«BLOCK_LEN»"},
		{0, token.ID, "x"},
		{0, token.PLUS, "+"},
		{0, token.INT, "1"},
		{0, token.NEWLINE, "\n"},
		{0, token.DEDENT, "« <- »"},

		{0, token.LPAREN, "("},
		{0, token.INT, "3"},
		{0, token.PLUS, "+"},
		{0, token.INT, "3"},
		{0, token.MINUS, "-"},
		{0, token.INT, "2"},
		{0, token.MULT, "*"},
		{0, token.INT, "1"},
		{0, token.DIV, "/"},
		{0, token.INT, "1"},
		{0, token.EXP, "^"},
		{0, token.INT, "2"},
		{0, token.MOD, "%"},
		{0, token.INT, "999"},
		{0, token.RPAREN, ")"},
		{0, token.DOT, "."},
		{0, token.ID, "foo"},
		{0, token.NEWLINE, "\n"},

		{0, token.MODULE, "module"},
		{0, token.EXPORT, "export"},
		{0, token.LET, "let"},
		{0, token.CASE, "case"},
		{0, token.LBRACKET, "["},
		{0, token.LBRACE, "{"},
		{0, token.RBRACE, "}"},
		{0, token.RBRACKET, "]"},
		{0, token.NEWLINE, "\n"},

		{0, token.ID, "a"},
		{0, token.OR, "|"},
		{0, token.ID, "日本語"},
		{0, token.AND, "&"},
		{0, token.ID, "c"},
		{0, token.LTE, "<="},
		{0, token.NOT, "!"},
		{0, token.ID, "d"},
		{0, token.LT, "<"},
		{0, token.GTE, ">="},
		{0, token.GT, ">"},
		{0, token.NEQ, "!="},
		{0, token.EQUAL, "=="},
		{0, token.COMMA, ","},
		{0, token.COLON, ":"},
		{0, token.SEMICOLON, ";"},
		{0, token.EOF, "«EOF»"},
	}

	l := New(input)

	var numNewlines, numIndents, numDedents, sumBlockLens int

	for i, tt := range tests {
		tok := l.NextToken()

		switch tok.Type {
		case token.NEWLINE:
			numNewlines++
		case token.INDENT:
			numIndents++
		case token.DEDENT:
			numDedents++
		case token.BLOCK_LEN:
			sumBlockLens += tok.NumLines
		}

		if tok.Literal != tt.expectedLiteral {
			t.Fatalf("tests[%d] - literal wrong. expected=%q, got=%q",
				i, tt.expectedLiteral, tok.Literal)
		}

		if tok.NumLines != tt.expectedNumLines {
			t.Fatalf("tests[%d] - numLines wrong. expected=%v, got=%v",
				i, tt.expectedNumLines, tok.NumLines)
		}

		if tok.Type != tt.expectedType {
			t.Fatalf("tests[%d] - tokentype wrong. expected=%q, got=%q",
				i, tt.expectedType, tok.Type)
		}
	}

	if numNewlines+numIndents+numDedents != sumBlockLens {
		t.Fatalf("newlines(%v)+indents(%v)+dedents(%v) != blockLens(%v)", numNewlines, numIndents, numDedents, sumBlockLens)
	}
}
