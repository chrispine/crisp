package token

import (
	"strings"
)

type TokenType int

type Token struct {
	Type     TokenType
	Literal  string
	NumLines int // only used for BLOCK_LEN tokens
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

	BLOCK_LEN // token to keep track of how many lines are in this block

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
		{LTE, strings.TrimSpace("          <=    "), 0},
		{GTE, strings.TrimSpace("          >=    "), 0},
		{EQUAL, strings.TrimSpace("        ==    "), 0},
		{NEQ, strings.TrimSpace("          !=    "), 0},
		{LT, strings.TrimSpace("           <     "), 0},
		{GT, strings.TrimSpace("           >     "), 0},

		{PATMAT, strings.TrimSpace("       =     "), 0},
		{ARROW, strings.TrimSpace("        ->    "), 0},
		{COLON, strings.TrimSpace("        :     "), 0},

		{DBLPLUS, strings.TrimSpace("      ++    "), 0},
		{DBLMINUS, strings.TrimSpace("     --    "), 0},
		{DBLMULT, strings.TrimSpace("      **    "), 0},
		{DBLDIV, strings.TrimSpace("       //    "), 0},
		{DBLMOD, strings.TrimSpace("       %%    "), 0},
		{DBLEXP, strings.TrimSpace("       ^^    "), 0},
		{DBLAND, strings.TrimSpace("       &&    "), 0},
		{DBLOR, strings.TrimSpace("        ||    "), 0},

		{PLUS, strings.TrimSpace("         +     "), 0},
		{MINUS, strings.TrimSpace("        -     "), 0},
		{MULT, strings.TrimSpace("         *     "), 0},
		{DIV, strings.TrimSpace("          /     "), 0},
		{MOD, strings.TrimSpace("          %     "), 0},
		{EXP, strings.TrimSpace("          ^     "), 0},
		{AND, strings.TrimSpace("          &     "), 0},
		{OR, strings.TrimSpace("           |     "), 0},
		{NOT, strings.TrimSpace("          !     "), 0},

		{TBLOCK, strings.TrimSpace("      (*)    "), 0},
		{LBLOCK, strings.TrimSpace("      [*]    "), 0},
		{LPAREN, strings.TrimSpace("      (      "), 0},
		{RPAREN, strings.TrimSpace("        )    "), 0},
		{LBRACKET, strings.TrimSpace("    [      "), 0},
		{RBRACKET, strings.TrimSpace("      ]    "), 0},
		{LBRACE, strings.TrimSpace("      {      "), 0},
		{RBRACE, strings.TrimSpace("        }    "), 0},

		{DOT, strings.TrimSpace("          .     "), 0},
		{AT, strings.TrimSpace("           @     "), 0},
		{COMMA, strings.TrimSpace("        ,     "), 0},
		{SEMICOLON, strings.TrimSpace("    ;     "), 0},
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
