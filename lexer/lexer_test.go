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

a|日本語&c<=!d<>=> == ,:;`

	tests := []struct {
		expectedType    token.TokenType
		expectedLiteral string
	}{
		{token.NEWLINE, "\n"},
		{token.ID, "foo"},
		{token.PATMAT, "="},
		{token.ID, "x"},
		{token.ARROW, "->"},
		{token.NEWLINE, "\n"},
		{token.INDENT, "\t"},
		{token.ID, "x"},
		{token.PLUS, "+"},
		{token.INT, "1"},
		{token.NEWLINE, "\n"},

		{token.NEWLINE, "\n"},
		{token.LPAREN, "("},
		{token.INT, "3"},
		{token.PLUS, "+"},
		{token.INT, "3"},
		{token.MINUS, "-"},
		{token.INT, "2"},
		{token.MULT, "*"},
		{token.INT, "1"},
		{token.DIV, "/"},
		{token.INT, "1"},
		{token.EXP, "^"},
		{token.INT, "2"},
		{token.MOD, "%"},
		{token.INT, "999"},
		{token.RPAREN, ")"},
		{token.DOT, "."},
		{token.ID, "foo"},
		{token.NEWLINE, "\n"},

		{token.NEWLINE, "\n"},
		{token.MODULE, "module"},
		{token.EXPORT, "export"},
		{token.LET, "let"},
		{token.CASE, "case"},
		{token.LBRACKET, "["},
		{token.LBRACE, "{"},
		{token.RBRACE, "}"},
		{token.RBRACKET, "]"},
		{token.NEWLINE, "\n"},

		{token.NEWLINE, "\n"},
		{token.ID, "a"},
		{token.OR, "|"},
		{token.ID, "日本語"},
		{token.AND, "&"},
		{token.ID, "c"},
		{token.LTE, "<="},
		{token.NOT, "!"},
		{token.ID, "d"},
		{token.LT, "<"},
		{token.GTE, ">="},
		{token.GT, ">"},
		{token.EQUAL, "=="},
		{token.COMMA, ","},
		{token.COLON, ":"},
		{token.SEMICOLON, ";"},
		{token.EOF, ""},
	}

	l := New(input)

	for i, tt := range tests {
		tok := l.NextToken()

		if tok.Literal != tt.expectedLiteral {
			t.Fatalf("tests[%d] - literal wrong. expected=%q, got=%q",
				i, tt.expectedLiteral, tok.Literal)
		}

		if tok.Type != tt.expectedType {
			t.Fatalf("tests[%d] - tokentype wrong. expected=%q, got=%q",
				i, tt.expectedType, tok.Type)
		}
	}
}
