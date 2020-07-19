package token

import (
	"strings"
)

type TokType int

type Token struct {
	NumLines int // only used for BlockLen tokens
	Type     TokType
	Literal  string
}

// let's create some tokens for our later convenience
var ExprBlockToken = Token{
	Type:    ExprBlock,
	Literal: "«ExprBlock»",
}
var LetToken = Token{
	Type:    Let,
	Literal: "let",
}
var AtToken = Token{
	Type:    At,
	Literal: "@",
}
var PatMatToken = Token{
	Type:    PatMat,
	Literal: "=",
}
var EqualToken = Token{
	Type:    Equal,
	Literal: "==",
}

const (
	Illegal TokType = iota

	ExprBlock // virtual token the parser needs; lexer does not emit this

	BlockLen // token to keep track of how many lines are in this block

	// Whitespace
	NewLine
	Indent // tabs are the only valid indent characters
	Dedent
	EOF

	// Identifiers (including ints and bools)
	ID
	NoMatch // _
	Shadow  // $
	Float

	// Operators
	TBlock // (*)
	RBlock // {*}
	LBlock // [*]
	PatMat // =
	Arrow  // ->
	Colon  // :

	DblPlus  // ++
	DblMinus // --
	DblMult  // **
	DblDiv   // //
	DblMod   // %%
	DblExp   // ^^
	DblAnd   // &&
	DblOr    // ||

	FPlus  // +.
	FMinus // -.
	FMult  // *.
	FDiv   // /.
	FMod   // %.
	FExp   // ^.
	Plus   // +
	Minus  // -
	Mult   // *
	Div    // /
	Mod    // %
	Exp    // ^
	And    // &
	Or     // |
	Not    // !

	LT    // <
	GT    // >
	LTE   // <=
	GTE   // >=
	FLT   // <.
	FGT   // >.
	FLTE  // <=.
	FGTE  // >=.
	Equal // ==
	NEq   // !=

	// Delimiters
	LParen   // (
	RParen   // )
	LBrace   // {
	RBrace   // }
	LBracket // [
	RBracket // ]

	Dot       // .
	At        // @
	Comma     // ,
	Semicolon // ;

	// Keywords
	Let
	Case
	Module
	Export

	// Unused
	NumTokenTypes
)

// Note that the order of these is significant:
// e.g. "->" must come before "-"
var Operators = []Token{
	{0, FLTE, strings.TrimSpace("         <=.   ")},
	{0, FGTE, strings.TrimSpace("         >=.   ")},
	{0, FLT, strings.TrimSpace("          <.    ")},
	{0, FGT, strings.TrimSpace("          >.    ")},
	{0, LTE, strings.TrimSpace("          <=    ")},
	{0, GTE, strings.TrimSpace("          >=    ")},
	{0, Equal, strings.TrimSpace("        ==    ")},
	{0, NEq, strings.TrimSpace("          !=    ")},
	{0, LT, strings.TrimSpace("           <     ")},
	{0, GT, strings.TrimSpace("           >     ")},

	{0, PatMat, strings.TrimSpace("       =     ")},
	{0, Shadow, strings.TrimSpace("       $     ")},
	{0, Arrow, strings.TrimSpace("        ->    ")},
	{0, Colon, strings.TrimSpace("        :     ")},

	{0, DblPlus, strings.TrimSpace("      ++    ")},
	{0, DblMinus, strings.TrimSpace("     --    ")},
	{0, DblMult, strings.TrimSpace("      **    ")},
	{0, DblDiv, strings.TrimSpace("       //    ")},
	{0, DblMod, strings.TrimSpace("       %%    ")},
	{0, DblExp, strings.TrimSpace("       ^^    ")},
	{0, DblAnd, strings.TrimSpace("       &&    ")},
	{0, DblOr, strings.TrimSpace("        ||    ")},

	{0, FPlus, strings.TrimSpace("        +.    ")},
	{0, FMinus, strings.TrimSpace("       -.    ")},
	{0, FMult, strings.TrimSpace("        *.    ")},
	{0, FDiv, strings.TrimSpace("         /.    ")},
	{0, FMod, strings.TrimSpace("         %.    ")},
	{0, FExp, strings.TrimSpace("         ^.    ")},

	{0, Plus, strings.TrimSpace("         +     ")},
	{0, Minus, strings.TrimSpace("        -     ")},
	{0, Mult, strings.TrimSpace("         *     ")},
	{0, Div, strings.TrimSpace("          /     ")},
	{0, Mod, strings.TrimSpace("          %     ")},
	{0, Exp, strings.TrimSpace("          ^     ")},
	{0, And, strings.TrimSpace("          &     ")},
	{0, Or, strings.TrimSpace("           |     ")},
	{0, Not, strings.TrimSpace("          !     ")},

	{0, TBlock, strings.TrimSpace("      (*)    ")},
	{0, RBlock, strings.TrimSpace("      {*}    ")},
	{0, LBlock, strings.TrimSpace("      [*]    ")},
	{0, LParen, strings.TrimSpace("      (      ")},
	{0, RParen, strings.TrimSpace("        )    ")},
	{0, LBrace, strings.TrimSpace("      {      ")},
	{0, RBrace, strings.TrimSpace("        }    ")},
	{0, LBracket, strings.TrimSpace("    [      ")},
	{0, RBracket, strings.TrimSpace("      ]    ")},

	{0, Dot, strings.TrimSpace("          .     ")},
	{0, At, strings.TrimSpace("           @     ")},
	{0, Comma, strings.TrimSpace("        ,     ")},
	{0, Semicolon, strings.TrimSpace("    ;     ")},
}

var keywords = map[string]TokType{
	"let":    Let,
	"case":   Case,
	"module": Module,
	"export": Export,
}

func LookupID(id string) TokType {
	if tok, ok := keywords[id]; ok {
		return tok
	}

	return ID
}
