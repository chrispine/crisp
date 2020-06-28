package lexer

import (
	"crisp/token"
	"unicode"
	"unicode/utf8"
)

type Lexer struct {
	input        string
	position     int  // current position in input (points to current char)
	readPosition int  // current reading position in input (after current char)
	ch           rune // current char under examination
	newline      bool // current position is at beginning of line
}

func New(input string) *Lexer {
	l := &Lexer{input: input}
	l.readRune()
	l.newline = true
	return l
}

func (l *Lexer) readRune() {
	if l.readPosition >= len(l.input) {
		l.ch = 0
		l.position = l.readPosition
		l.readPosition += 1
	} else {
		runeVal, runeW := utf8.DecodeRuneInString(l.input[l.readPosition:])
		l.ch = runeVal
		l.position = l.readPosition
		l.readPosition += runeW
	}
}

func (l *Lexer) NextToken() token.Token {
	var tok token.Token

	l.consumeWhitespace()
	l.consumeComment()

	switch l.ch {
	case '\t':
		if l.newline {
			tok = newToken(token.INDENT, string(l.ch))
			l.readRune()
			return tok
		} else {
			// TODO: better error reporting here for unexpected tabs
			tok = newToken(token.ILLEGAL, string(l.ch))
		}
	case 0:
		tok.Literal = ""
		tok.Type = token.EOF
	default:
		if isNewline(l.ch) {
			tok = newToken(token.NEWLINE, "\n")
			l.readRune()
			l.newline = true
			return tok
		}

		pTok := l.consumeOperatorOrRewind()
		if pTok != nil {
			tok = *pTok
			l.newline = false
			return tok
		}

		if isLetter(l.ch) {
			tok.Literal = l.readIdentifier()
			tok.Type = token.LookupID(tok.Literal)
			l.newline = false
			return tok
		}

		if isDigit(l.ch) {
			tok.Type = token.INT
			tok.Literal = l.readNumber()
			l.newline = false
			return tok
		}

		tok = newToken(token.ILLEGAL, string(l.ch))
	}

	l.readRune()
	l.newline = false
	return tok
}

func newToken(tokenType token.TokenType, str string) token.Token {
	return token.Token{Type: tokenType, Literal: str}
}

func (l *Lexer) readIdentifier() string {
	position := l.position

	for isLetter(l.ch) {
		l.readRune()
	}

	return l.input[position:l.position]
}

func (l *Lexer) readNumber() string {
	position := l.position

	for isDigit(l.ch) {
		l.readRune()
	}

	return l.input[position:l.position]
}

func (l *Lexer) consumeWhitespace() {
	for l.ch == ' ' || l.ch == 0x85 || l.ch == 0xA0 { // NEL, NBSP
		l.readRune()
	}
}

func (l *Lexer) consumeComment() {
	if l.ch == '#' {
		for !isNewline(l.ch) && l.ch > 0 {
			l.readRune()
		}
	}
}

func (l *Lexer) consumeOperatorOrRewind() *token.Token {
	for _, tok := range token.GetOperators() {
		opStr := tok.Literal

		if len(l.input) >= l.position+len(opStr) &&
			l.input[l.position:l.position+len(opStr)] == opStr {
			// advance the counter
			for i := 0; i < len(opStr); i++ {
				l.readRune()
			}
			return &tok
		}
	}

	// none found, so return nil
	return nil
}

func isLetter(ch rune) bool {
	return unicode.IsLetter(ch)
}

func isDigit(ch rune) bool {
	return '0' <= ch && ch <= '9'
}

func isNewline(ch rune) bool {
	return ch == '\n' || ch == '\r' || ch == '\v' || ch == '\f'
}
