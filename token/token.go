package token

import (
	"strings"
)

type TokenType int

type Token struct {
	Type    TokenType
	Literal string
}

const (
	ILLEGAL TokenType = iota

	// Whitespace
	NEWLINE
	INDENT // tabs are the only valid indent characters
	DEDENT
	EOF

	// Identifiers and ints
	ID
	INT

	// Operators
	PATMAT // =
	ARROW  // ->
	COLON  // :

	PLUS  // +
	MINUS // -
	MULT  // +
	DIV   // /
	MOD   // %
	EXP   // ^
	AND   // &
	OR    // |
	NOT   // !

	LT    // <
	GT    // >
	LTE   // <=
	GTE   // >=
	EQUAL // ==
	NEQ   // !=

	// Delimiters
	LPAREN   // (
	RPAREN   // )
	LBRACKET // [
	RBRACKET // ]
	LBRACE   // {
	RBRACE   // }

	DOT       // .
	COMMA     // ,
	SEMICOLON // ;

	// Keywords
	LET
	CASE
	MODULE
	EXPORT
)

// Note that the order of these is significant:
// e.g. "->" must come before "-"
func GetOperators() []Token {
	return []Token{
		{LTE, strings.TrimSpace("          <=    ")},
		{GTE, strings.TrimSpace("          >=    ")},
		{EQUAL, strings.TrimSpace("        ==    ")},
		{NEQ, strings.TrimSpace("          !=    ")},
		{LT, strings.TrimSpace("           <     ")},
		{GT, strings.TrimSpace("           >     ")},

		{PATMAT, strings.TrimSpace("       =     ")},
		{ARROW, strings.TrimSpace("        ->    ")},
		{COLON, strings.TrimSpace("        :     ")},

		{PLUS, strings.TrimSpace("         +     ")},
		{MINUS, strings.TrimSpace("        -     ")},
		{MULT, strings.TrimSpace("         *     ")},
		{DIV, strings.TrimSpace("          /     ")},
		{MOD, strings.TrimSpace("          %     ")},
		{EXP, strings.TrimSpace("          ^     ")},
		{AND, strings.TrimSpace("          &     ")},
		{OR, strings.TrimSpace("           |     ")},
		{NOT, strings.TrimSpace("          !     ")},

		{LPAREN, strings.TrimSpace("      (      ")},
		{RPAREN, strings.TrimSpace("        )    ")},
		{LBRACKET, strings.TrimSpace("    [      ")},
		{RBRACKET, strings.TrimSpace("      ]    ")},
		{LBRACE, strings.TrimSpace("      {      ")},
		{RBRACE, strings.TrimSpace("        }    ")},

		{DOT, strings.TrimSpace("          .     ")},
		{COMMA, strings.TrimSpace("        ,     ")},
		{SEMICOLON, strings.TrimSpace("    ;     ")},
	}
}

var keywords = map[string]TokenType{
	"let":    LET,
	"case":   CASE,
	"module": MODULE,
	"export": EXPORT,
}

func LookupID(id string) TokenType {
	if tok, ok := keywords[id]; ok {
		return tok
	}

	return ID
}
