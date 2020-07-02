package parser

import (
	"crisp/ast"
	"crisp/lexer"
	"crisp/token"
	"fmt"
	"reflect"
	"strconv"
)

var unops = []token.TokenType{
	token.MINUS, //  -
	token.NOT,   //  !
}

/*
(true ,   |   ||
(true ,   &   &&
(true ,   ==  !=
(true ,   <   <=  >   >=
(true ,   +   ++  -   --
(true ,   *   **  /   //  %   %%
(false,   ^   ^^
*/

type (
	parseUnopFn  func() ast.Inline
	parseBinopFn func(ast.Inline) ast.Inline
)

type Parser struct {
	l      *lexer.Lexer
	errors []string

	unit *ast.InlineTuple

	curToken  token.Token
	peekToken token.Token

	parseUnopFns  map[token.TokenType]parseUnopFn
	parseBinopFns map[token.TokenType]parseBinopFn
}

func New(l *lexer.Lexer) *Parser {
	p := &Parser{
		l:      l,
		errors: []string{},
		unit: &ast.InlineTuple{
			Token: token.Token{Type: token.LPAREN, Literal: "("},
		},
	}

	p.parseUnopFns = make(map[token.TokenType]parseUnopFn)
	p.parseBinopFns = make(map[token.TokenType]parseBinopFn)

	// Read two tokens, so curToken and peekToken are both set
	p.nextToken()
	p.nextToken()

	return p
}

func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.l.NextToken()
}

func (p *Parser) expectToken(ts ...token.TokenType) {
	for _, t := range ts {
		if p.curTokenIs(t) {
			p.nextToken()
			return
		}
	}

	p.error(fmt.Sprintf("expected next token to be in %v, got %v instead", ts, p.peekToken))
}

func (p *Parser) curTokenIs(t token.TokenType) bool {
	return p.curToken.Type == t
}

func (p *Parser) peekTokenIs(t token.TokenType) bool {
	return p.peekToken.Type == t
}

func (p *Parser) Errors() []string {
	return p.errors
}

func (p *Parser) error(err string) {
	p.errors = append(p.errors, err)
}

func (p *Parser) noPrefixParseFnError(t token.TokenType) {
	p.error(fmt.Sprintf("no prefix parse function for %v found", t))
}

/*
☉Atom ->
	ID
	Int
	Bool
	Tuple
	List
	UnopExpr
*/
func (p *Parser) parseAtom() ast.Inline {
	switch p.curToken.Type {
	case token.ID:
		return p.parseID()
	case token.INT:
		return p.parseInt()
	case token.TRUE, token.FALSE:
		return p.parseBool()
	case token.LPAREN:
		return p.parseTuple()
	case token.LBRACKET:
		return p.parseList()
	}

	return p.parseUnopExpr()
}

func (p *Parser) parseID() *ast.InlineID {
	lit := &ast.InlineID{Token: p.curToken, Name: p.curToken.Literal}

	p.expectToken(token.ID)

	return lit
}

func (p *Parser) parseInt() *ast.InlineInt {
	lit := &ast.InlineInt{Token: p.curToken}

	i, err := strconv.ParseInt(p.curToken.Literal, 0, 0)
	if !isNil(err) {
		p.error(fmt.Sprintf("failed to parse %q as integer", p.curToken.Literal))
		return nil
	}

	lit.Value = int(i)

	p.expectToken(token.INT)

	return lit
}

func (p *Parser) parseBool() *ast.InlineBool {
	lit := &ast.InlineBool{Token: p.curToken}

	lit.Value = p.curToken.Type == token.TRUE

	p.expectToken(token.TRUE, token.FALSE)

	return lit
}

/*
Tuple ->
	'('  ')'
	'('  Expr  (','  Expr)*  ')'

Note that there are no 1-tuples, as those are just parenthesized
expressions. In that case, parseTuple may not return a *ast.InlineTuple.
*/
func (p *Parser) parseTuple() ast.Inline {
	lit := &ast.InlineTuple{Token: p.curToken}

	p.expectToken(token.LPAREN)

	// check for unit (empty tuple)
	if p.curToken.Type == token.RPAREN {
		p.expectToken(token.RPAREN)
		return p.unit
	}

	exprs := []ast.Inline{}
Loop:
	for {
		exprs = append(exprs, p.ParseExpr())

		switch p.curToken.Type {
		case token.RPAREN:
			p.expectToken(token.RPAREN)
			break Loop
		case token.COMMA:
			p.expectToken(token.COMMA)
		default:
			p.error(fmt.Sprintf("malformed tuple: found token %v", p.curToken))
			return nil
		}
	}

	if len(exprs) == 1 {
		// no such thing as a 1-tuple
		return exprs[0]
	}

	lit.Exprs = exprs

	return lit
}

/*
List ->
	'['  ']'
	'['  Expr  (','  Expr)*  (';'  Expr)?  ']'

Note that this is a recursive function.
*/
func (p *Parser) parseList() *ast.InlineCons {
	lit := &ast.InlineCons{Token: p.curToken}

	p.expectToken(token.LBRACKET, token.COMMA)

	// check for nil (empty list)
	if p.curToken.Type == token.RBRACKET {
		p.expectToken(token.RBRACKET)
		return nil
	}

	lit.Head = p.ParseExpr()

	if p.curToken.Type == token.SEMICOLON {
		p.expectToken(token.SEMICOLON)
		lit.Tail = p.ParseExpr()
		p.expectToken(token.RBRACKET)

		return lit
	}

	// check for end of list
	if p.curToken.Type == token.RBRACKET {
		p.expectToken(token.RBRACKET)
		lit.Tail = nil
		return lit
	}

	lit.Tail = p.parseList()

	return lit
}

/*
UnopExpr ->
	[-!]  Atom
*/

func (p *Parser) parseUnopExpr() *ast.InlineUnopExpr {
	for _, op := range unops {
		if p.curToken.Type == op {
			lit := &ast.InlineUnopExpr{Token: p.curToken}
			p.expectToken(op)
			lit.Expr = p.parseAtom()

			return lit
		}
	}

	p.error(fmt.Sprintf("failed to parse %v as atom", p.curToken))
	return nil
}

/*
☉Expr ->
	Atom
	BinopExpr
	FuncExpr
	ApplyExpr
*/
func (p *Parser) ParseExpr() ast.Inline { // TODO: don't export this
	return p.parseExpression(0)
}
func (p *Parser) parseExpression(precedence int) ast.Inline {
	return p.parseAtom()
}

/*

BinopExpr ->
	Expr  [+-*%/^&|.@]  Expr

FuncExpr ->
	LvalAtom  '->'  Expr

ApplyExpr ->
	Expr  '@'  Expr
	Expr  '.'  Expr
	Expr  Expr

*/

func (p *Parser) registerUnop(tokenType token.TokenType, fn parseUnopFn) {
	p.parseUnopFns[tokenType] = fn
}

func (p *Parser) registerBinop(tokenType token.TokenType, fn parseBinopFn) {
	p.parseBinopFns[tokenType] = fn
}

func isNil(i interface{}) bool {
	return i == nil || reflect.ValueOf(i).IsNil()
}
