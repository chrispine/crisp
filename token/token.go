package token

import (
	"strings"
)

type TokenType int

type Token struct {
	Type    TokenType
	Literal string
}

var ExprBlockToken = Token{
	Type:    EXPR_BLOCK,
	Literal: "«ExprBlock»",
}

var AtToken = Token{
	Type:    AT,
	Literal: "@",
}

const (
	ILLEGAL TokenType = iota

	EXPR_BLOCK // virtual token the parser needs; lexer does not emit this

	// Whitespace
	NEWLINE
	INDENT // tabs are the only valid indent characters
	DEDENT
	EOF

	// Identifiers and ints
	ID
	INT

	// Operators
	TBLOCK // (*)
	LBLOCK // [*]
	PATMAT // =
	ARROW  // ->
	COLON  // :

	DBLPLUS  // ++
	DBLMINUS // --
	DBLMULT  // **
	DBLDIV   // //
	DBLMOD   // %%
	DBLEXP   // ^^
	DBLAND   // &&
	DBLOR    // ||

	PLUS  // +
	MINUS // -
	MULT  // *
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
	AT        // @
	COMMA     // ,
	SEMICOLON // ;

	// Keywords
	LET
	CASE
	MODULE
	EXPORT
	TRUE
	FALSE

	// Unused
	NUM_TOKEN_TYPES
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

		{DBLPLUS, strings.TrimSpace("      ++    ")},
		{DBLMINUS, strings.TrimSpace("     --    ")},
		{DBLMULT, strings.TrimSpace("      **    ")},
		{DBLDIV, strings.TrimSpace("       //    ")},
		{DBLMOD, strings.TrimSpace("       %%    ")},
		{DBLEXP, strings.TrimSpace("       ^^    ")},
		{DBLAND, strings.TrimSpace("       &&    ")},
		{DBLOR, strings.TrimSpace("        ||    ")},

		{PLUS, strings.TrimSpace("         +     ")},
		{MINUS, strings.TrimSpace("        -     ")},
		{MULT, strings.TrimSpace("         *     ")},
		{DIV, strings.TrimSpace("          /     ")},
		{MOD, strings.TrimSpace("          %     ")},
		{EXP, strings.TrimSpace("          ^     ")},
		{AND, strings.TrimSpace("          &     ")},
		{OR, strings.TrimSpace("           |     ")},
		{NOT, strings.TrimSpace("          !     ")},

		{TBLOCK, strings.TrimSpace("      (*)    ")},
		{LBLOCK, strings.TrimSpace("      [*]    ")},
		{LPAREN, strings.TrimSpace("      (      ")},
		{RPAREN, strings.TrimSpace("        )    ")},
		{LBRACKET, strings.TrimSpace("    [      ")},
		{RBRACKET, strings.TrimSpace("      ]    ")},
		{LBRACE, strings.TrimSpace("      {      ")},
		{RBRACE, strings.TrimSpace("        }    ")},

		{DOT, strings.TrimSpace("          .     ")},
		{AT, strings.TrimSpace("           @     ")},
		{COMMA, strings.TrimSpace("        ,     ")},
		{SEMICOLON, strings.TrimSpace("    ;     ")},
	}
}

var keywords = map[string]TokenType{
	"let":    LET,
	"case":   CASE,
	"module": MODULE,
	"export": EXPORT,
	"true":   TRUE,
	"false":  FALSE,
}

func LookupID(id string) TokenType {
	if tok, ok := keywords[id]; ok {
		return tok
	}

	return ID
}
