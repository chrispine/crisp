package lexer

import (
	"crisp/token"
	"strconv"
	"unicode"
	"unicode/utf8"
)

type Lexer struct {
	input         string
	position      int  // current position in input (points to current char)
	readPosition  int  // current reading position in input (after current char)
	ch            rune // current char under examination
	atStartOfLine bool // current position is at beginning of line
	indentation   int  // indentation depth of this line
}

func New(input string) *Lexer {
	l := &Lexer{input: input}
	l.readRune()
	l.atStartOfLine = true
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

	if l.atStartOfLine {
		newIndent := l.consumeIndentation()
		delta := newIndent - l.indentation
		l.atStartOfLine = false

		switch {
		case delta == 1:
			tok = newToken(token.INDENT, "« -> »")
			l.indentation++
			return tok
		case delta == -1:
			tok = newToken(token.DEDENT, "« <- »")
			l.indentation--
			return tok
		case delta > 1 || delta < -1:
			// we only allow increasing or decreasing indentation levels by 1
			tok = newToken(token.ILLEGAL, "«INDENT "+strconv.Itoa(delta)+"»")
			return tok
		}
		// If we got here, the indendation level didn't change, so carry on.
	}

	l.consumeWhitespace()
	l.consumeComment()

	if l.ch == 0 {
		tok.Literal = "«EOF»"
		tok.Type = token.EOF
		l.readRune()
		return tok
	}

	if isNewline(l.ch) {
		tok = newToken(token.NEWLINE, "\n")
		l.atStartOfLine = true
		l.readRune()
		return tok
	}

	pTok := l.consumeOperatorOrRewind()
	if pTok != nil {
		tok = *pTok
		return tok
	}

	if isLetter(l.ch) {
		tok.Literal = l.readIdentifier()
		tok.Type = token.LookupID(tok.Literal)
		return tok
	}

	if isDigit(l.ch) {
		tok.Type = token.INT
		tok.Literal = l.readNumber()
		return tok
	}

	// ¯\_(ツ)_/¯
	tok = newToken(token.ILLEGAL, string(l.ch))
	l.readRune()
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

// consumes indentation, returns indentation level
// NOTE: If this line is blank or a comment, we consume
// until the next line
func (l *Lexer) consumeIndentation() int {
	indent := 0

	for l.ch == '\t' {
		indent++
		l.readRune()
	}

	if !isWhitespace(l.ch) && l.ch != '#' {
		return indent
	}

	// Maybe this is a blank line, so we'll see.
	l.consumeWhitespace()
	l.consumeComment()

	if isNewline(l.ch) {
		l.readRune()
		return l.consumeIndentation()
	}

	// If we got here, it means we had a line with indentation (tabs),
	// followed by whitespace, and then real code. Could be mixing tabs
	// and spaces, could be just weird code. We might need to change
	// this if we find valid use-cases for "weird code".
	return -99
}

func (l *Lexer) consumeWhitespace() {
	for isWhitespace(l.ch) {
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

func isLetter(ch rune) bool {
	return unicode.IsLetter(ch)
}

func isDigit(ch rune) bool {
	return '0' <= ch && ch <= '9'
}

func isWhitespace(ch rune) bool {
	return ch == ' ' || ch == '\t' || ch == 0x85 || ch == 0xA0 // NEL, NBSP
}

func isNewline(ch rune) bool {
	return ch == '\n' || ch == '\r' || ch == '\v' || ch == '\f'
}
