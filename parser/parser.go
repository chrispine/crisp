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

var isBinop, isEndOfExpr map[token.TokenType]bool

type BinopPrecList struct {
	lAssoc bool
	ops    []token.TokenType
}

func (bpl *BinopPrecList) Contains(tok token.TokenType) bool {
	for _, tokenType := range bpl.ops {
		if tok == tokenType {
			return true
		}
	}

	return false
}

// NOTE: this holds the binops *in order of precedence*
var binopPrecs = []BinopPrecList{
	{lAssoc: true, ops: []token.TokenType{ //  |  ||
		token.OR,
		token.DBLOR,
	}},
	{lAssoc: true, ops: []token.TokenType{ //  &  &&
		token.AND,
		token.DBLAND,
	}},
	{lAssoc: true, ops: []token.TokenType{ //  ==  !=
		token.EQUAL,
		token.NEQ,
	}},
	{lAssoc: true, ops: []token.TokenType{ //  <  <=  >  >=
		token.LT,
		token.LTE,
		token.GT,
		token.GTE,
	}},
	{lAssoc: true, ops: []token.TokenType{ //  +  ++  -  --
		token.PLUS,
		token.DBLPLUS,
		token.MINUS,
		token.DBLMINUS,
	}},
	{lAssoc: true, ops: []token.TokenType{ //  *  **  /  //  %  %%
		token.MULT,
		token.DBLMULT,
		token.DIV,
		token.DBLDIV,
		token.MOD,
		token.DBLMOD,
	}},
	{lAssoc: false, ops: []token.TokenType{ //  ^  ^^
		token.EXP,
		token.DBLEXP,
	}},
}

type Parser struct {
	l      *lexer.Lexer
	errors []string

	unit *ast.InlineTuple

	curToken  *token.Token
	peekToken *token.Token
}

func New(l *lexer.Lexer) *Parser {
	p := &Parser{
		l:      l,
		errors: []string{},
		unit: &ast.InlineTuple{
			Token: token.Token{Type: token.LPAREN, Literal: "("},
		},
	}

	isBinop = calculateBinopMap()
	isEndOfExpr = calculateEndOfExprMap()

	// Read two tokens, so curToken and peekToken are both set
	p.nextToken()
	p.nextToken()

	return p
}

func calculateBinopMap() map[token.TokenType]bool {
	bops := map[token.TokenType]bool{}

	// first set all token types to false
	for i := token.TokenType(0); i < token.NUM_TOKEN_TYPES; i++ {
		bops[i] = false
	}

	// then set all of the binary ops to true
	for _, bp := range binopPrecs {
		for _, op := range bp.ops {
			bops[op] = true
		}
	}

	return bops
}

func calculateEndOfExprMap() map[token.TokenType]bool {
	eoes := map[token.TokenType]bool{}

	// first set all token types to false
	for i := token.TokenType(0); i < token.NUM_TOKEN_TYPES; i++ {
		eoes[i] = false
	}

	// then set these to true
	eoes[token.EOF] = true
	eoes[token.COMMA] = true
	eoes[token.SEMICOLON] = true
	eoes[token.RPAREN] = true
	eoes[token.RBRACKET] = true
	eoes[token.RBRACE] = true
	eoes[token.NEWLINE] = true
	// TODO: also add DEDENT? INDENT??

	return eoes
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
	panic(err) // TODO: remove this line when parser is stable
}

func (p *Parser) noPrefixParseFnError(t token.TokenType) {
	p.error(fmt.Sprintf("no prefix parse function for %v found", t))
}

/*
Program ->
	DeclsAndExpr
*/
func (p *Parser) ParseProgram() *ast.Program {
	decls, expr := p.parseDeclsAndExpr()

	program := &ast.Program{
		Decls: decls,
		Expr:  expr,
	}

	// scan for unconsumed tokens
	var leftoverTokens []*token.Token
	for p.curToken.Type != token.EOF {
		leftoverTokens = append(leftoverTokens, p.curToken)
		p.nextToken()
	}

	if len(leftoverTokens) > 0 {
		p.error(fmt.Sprintf("failed to consume all tokens; tokens remaining: %v", leftoverTokens))
	}

	return program
}

/*
DeclsAndExpr ->
	«BLOCK_LEN»  DeclBlock*  ExprBlock
*/
func (p *Parser) parseDeclsAndExpr() ([]ast.Block, ast.Block) {
	var decls []ast.Block

	numDecls := p.curToken.NumLines - 1
	p.expectToken(token.BLOCK_LEN)

	for i := 0; i < numDecls; i++ {
		decls = append(decls, p.parseDeclBlock())
	}

	return decls, p.parseExprBlock()
}

/*
☉DeclBlock ->
	PatMatBlock
	FuncDeclBlock
*/
func (p *Parser) parseDeclBlock() ast.Block {
	atom := p.parseAtom()

	if p.curToken.Type == token.PATMAT {
		return p.parsePatMatBlock(atom)
	}

	return p.parseFuncDeclBlock(atom)
}

/*
PatMatBlock ->
	LvalAtom  '='  ExprBlock
*/
func (p *Parser) parsePatMatBlock(atom ast.Inline) ast.Block {
	if !atom.IsLval() {
		p.error(fmt.Sprintf("atom %v is not an l-value", atom))
	}

	lit := &ast.PatMatBlock{Token: *p.curToken, Lval: atom}

	p.expectToken(token.PATMAT)

	lit.Expr = p.parseExprBlock()

	return lit
}

/*
FuncDeclBlock ->
	ID  (LvalAtom)*  FuncBlock  // sugar for currying and 'let'
*/
func (p *Parser) parseFuncDeclBlock(atom ast.Inline) ast.Block {
	return nil // TODO
}

/*
☉ExprBlock ->
	JustExprBlock
	LetBlock
	FuncBlock
	TupleBlock
	ListBlock
*/
func (p *Parser) parseExprBlock() ast.Block {
	switch p.curToken.Type {
	case token.LET:
		return p.parseLetBlock()
	case token.TBLOCK:
		return p.parseTupleBlock()
	case token.LBLOCK:
		return p.parseListBlock()
	}
	// so it's either a FuncBlock or a JustExprBlock
	atom := p.parseAtom()

	if p.curToken.Type == token.ARROW {
		return p.parseFuncBlock(atom)
	}

	return p.parseJustExprBlock(atom)
}

/*
JustExprBlock ->
		Expr  '\n'                   // TODO: allow (by collapsing) multi-line expr
*/
func (p *Parser) parseJustExprBlock(atom ast.Inline) ast.Block {
	lit := &ast.JustExprBlock{
		Token: token.ExprBlockToken,
		Expr:  p.parseExprRest(atom),
	}

	if p.curToken.Type != token.EOF {
		p.expectToken(token.NEWLINE)
	}

	return lit
}

/*
LetBlock ->
	'let'  '|->'  DeclsAndExpr  '<-|'
*/
func (p *Parser) parseLetBlock() ast.Block {
	lit := &ast.LetBlock{
		Token: *p.curToken,
	}
	p.expectToken(token.LET)
	p.expectToken(token.INDENT)

	lit.Decls, lit.Expr = p.parseDeclsAndExpr()

	return lit
}

/*
FuncBlock ->
	LvalAtom  '->'  ExprBlock
	LvalAtom  '->'  |->'  DeclsAndExpr  '<-|'                  // sugar for 'let'
*/
func (p *Parser) parseFuncBlock(atom ast.Inline) ast.Block {
	p.expectToken(token.ARROW)

	if !atom.IsLval() {
		p.error(fmt.Sprintf("atom %v is not an l-value", atom))
	}

	lit := &ast.FuncBlock{
		Token: *p.curToken,
		Lval:  atom,
	}

	if p.curToken.Type == token.INDENT {
		p.expectToken(token.INDENT)
		letBlock := &ast.LetBlock{Token: token.Token{
			Type:    token.LET,
			Literal: "let",
		}}
		letBlock.Decls, letBlock.Expr = p.parseDeclsAndExpr()
		lit.Expr = letBlock
	} else {
		lit.Expr = p.parseExprBlock()
	}

	return lit
}

/*
TupleBlock ->
	'(*)'  |->'  ExprBlock+  '<-|'
*/
func (p *Parser) parseTupleBlock() ast.Block {
	lit := &ast.TupleBlock{Token: *p.curToken}
	p.expectToken(token.TBLOCK)
	p.expectToken(token.INDENT)
	numElems := p.curToken.NumLines
	p.expectToken(token.BLOCK_LEN)

	if numElems <= 0 {
		p.error(fmt.Sprintf("invalide TupleBlock parsed with %v elements", numElems))
	}
	for i := 0; i < numElems; i++ {
		lit.Exprs = append(lit.Exprs, p.parseExprBlock())
	}

	p.expectToken(token.DEDENT)

	return lit
}

/*
ListBlock ->
	'[*]'  |->'  ExprBlock+  (';'  ExprBlock)?  '<-|'
*/
func (p *Parser) parseListBlock() ast.Block {
	// TODO: parse this
	return nil
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
	lit := &ast.InlineID{Token: *p.curToken, Name: p.curToken.Literal}

	p.expectToken(token.ID)

	return lit
}

func (p *Parser) parseInt() *ast.InlineInt {
	lit := &ast.InlineInt{Token: *p.curToken}

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
	lit := &ast.InlineBool{Token: *p.curToken}

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
	lit := &ast.InlineTuple{Token: *p.curToken}

	p.expectToken(token.LPAREN)

	// check for unit (empty tuple)
	if p.curToken.Type == token.RPAREN {
		p.expectToken(token.RPAREN)
		return p.unit
	}

	var exprs []ast.Inline
Loop:
	for {
		exprs = append(exprs, p.parseExpr())

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
	lit := &ast.InlineCons{Token: *p.curToken}

	p.expectToken(token.LBRACKET, token.COMMA)

	// check for nil (empty list)
	if p.curToken.Type == token.RBRACKET {
		p.expectToken(token.RBRACKET)
		return nil
	}

	lit.Head = p.parseExpr()

	if p.curToken.Type == token.SEMICOLON {
		p.expectToken(token.SEMICOLON)
		lit.Tail = p.parseExpr()
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
			lit := &ast.InlineUnopExpr{Token: *p.curToken}
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

BinopExpr ->
	Expr  [+-*%/^&|]  Expr

FuncExpr ->
	LvalAtom  '->'  Expr

ApplyExpr ->
	Expr  '@'  Expr
	Expr  '.'  Expr
	Expr  Expr
*/
func (p *Parser) parseExpr() ast.Inline {
	return p.parseExpression(0)
}
func (p *Parser) parseExprRest(atom ast.Inline) ast.Inline {
	return p.parseExpressionRest(0, atom)
}
func (p *Parser) parseExpression(precedence int) ast.Inline {
	return p.parseExpressionRest(precedence, p.parseAtom())
}
func (p *Parser) parseExpressionRest(precedence int, atom ast.Inline) ast.Inline {
	if precedence >= len(binopPrecs) {
		if p.curToken.Type == token.ARROW {
			tok := p.curToken
			p.expectToken(token.ARROW)

			if !atom.IsLval() {
				p.error(fmt.Sprintf("atom %v is not an l-value", atom))
			}

			return &ast.InlineFunc{
				Token: *tok,
				Lval:  atom,
				Expr:  p.parseExpr(), // note that this resets the precedence; this is why token.ARROW is not treated as just another binop
			}
		}

		// Now what? Depends on what the next token is.
		// If it's a binop, or otherwise ends the expression,
		// then we should return.
		if isBinop[p.curToken.Type] || isEndOfExpr[p.curToken.Type] {
			return atom
		}

		// If we got here, then the next token indicates that
		// this should be a function call.
		return p.parseFuncsAndApplyExprs(atom)
	}

	higherPrecAST := p.parseExpressionRest(precedence+1, atom)

	if binopPrecs[precedence].Contains(p.curToken.Type) {
		tok := p.curToken

		if binopPrecs[precedence].lAssoc {
			return p.parseExpressionLeft(precedence, higherPrecAST)
		}

		p.nextToken()

		samePrecAST := p.parseExpression(precedence)

		return &ast.InlineBinopExpr{
			Token: *tok,
			LExpr: higherPrecAST,
			RExpr: samePrecAST,
		}
	}

	return higherPrecAST
}

func (p *Parser) parseExpressionLeft(precedence int, prevAST ast.Inline) ast.Inline {
	tok := p.curToken
	p.nextToken()

	higherPrecAST := p.parseExpression(precedence + 1)

	samePrecAST := &ast.InlineBinopExpr{
		Token: *tok,
		LExpr: prevAST,
		RExpr: higherPrecAST,
	}

	if binopPrecs[precedence].Contains(p.curToken.Type) {
		return p.parseExpressionLeft(precedence, samePrecAST)
	}

	return samePrecAST
}

func (p *Parser) parseFuncsAndApplyExprs(prevAST ast.Inline) ast.Inline {
	var fn, arg ast.Inline

	if p.curToken.Type == token.DOT {
		p.expectToken(token.DOT)
		fn = p.parseAtom()
		arg = prevAST
	} else {
		fn = prevAST
		arg = p.parseAtom()
	}

	applyExpr := &ast.InlineBinopExpr{
		Token: token.AtToken,
		LExpr: fn,
		RExpr: arg,
	}

	if isBinop[p.curToken.Type] || isEndOfExpr[p.curToken.Type] {
		return applyExpr
	}

	// TODO: more arrow handling here?

	return p.parseFuncsAndApplyExprs(applyExpr)
}

func isNil(i interface{}) bool {
	return i == nil || reflect.ValueOf(i).IsNil()
}
