package parser

import (
	"crisp/lexer"
	"crisp/parse_tree"
	"crisp/token"
	"fmt"
	"reflect"
)

var unops = []token.TokType{
	token.Minus, //  -
	token.Not,   //  !
}

var isBinop, isEndOfExpr map[token.TokType]bool

type BinopPrecList struct {
	lAssoc bool
	ops    []token.TokType
}

func (bpl *BinopPrecList) Contains(tok token.TokType) bool {
	for _, tokenType := range bpl.ops {
		if tok == tokenType {
			return true
		}
	}

	return false
}

// NOTE: this holds the binops *in order of precedence*
var binopPrecs = []BinopPrecList{
	{lAssoc: true, ops: []token.TokType{ //  |  ||
		token.Or,
		token.DblOr,
	}},
	{lAssoc: true, ops: []token.TokType{ //  &  &&
		token.And,
		token.DblAnd,
	}},
	{lAssoc: true, ops: []token.TokType{ //  ==  !=
		token.Equal,
		token.NEq,
	}},
	{lAssoc: true, ops: []token.TokType{ //  <  <=  >  >=
		token.LT,
		token.LTE,
		token.GT,
		token.GTE,
	}},
	{lAssoc: true, ops: []token.TokType{ //  +  ++  -  --
		token.Plus,
		token.DblPlus,
		token.Minus,
		token.DblMinus,
	}},
	{lAssoc: true, ops: []token.TokType{ //  *  **  /  //  %  %%
		token.Mult,
		token.DblMult,
		token.Div,
		token.DblDiv,
		token.Mod,
		token.DblMod,
	}},
	{lAssoc: false, ops: []token.TokType{ //  ^  ^^
		token.Exp,
		token.DblExp,
	}},
}

type Parser struct {
	l      *lexer.Lexer
	errors []string

	unit *parse_tree.InlineTuple

	curToken *token.Token
}

func New(l *lexer.Lexer) *Parser {
	p := &Parser{
		l:      l,
		errors: []string{},
		unit: &parse_tree.InlineTuple{
			Token: token.Token{Type: token.LParen, Literal: "("},
		},
	}

	isBinop = calculateBinopMap()
	isEndOfExpr = calculateEndOfExprMap()

	// read in first token
	p.nextToken()

	return p
}

func calculateBinopMap() map[token.TokType]bool {
	bops := map[token.TokType]bool{}

	// first set all token types to false
	for i := token.TokType(0); i < token.NumTokenTypes; i++ {
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

func calculateEndOfExprMap() map[token.TokType]bool {
	eoes := map[token.TokType]bool{}

	// first set all token types to false
	for i := token.TokType(0); i < token.NumTokenTypes; i++ {
		eoes[i] = false
	}

	// then set these to true
	eoes[token.EOF] = true
	eoes[token.Comma] = true
	eoes[token.Semicolon] = true
	eoes[token.RParen] = true
	eoes[token.RBracket] = true
	eoes[token.RBrace] = true
	eoes[token.NewLine] = true
	// TODO: also add Dedent? Indent??

	return eoes
}

func (p *Parser) nextToken() {
	p.curToken = p.l.NextToken()
}

func (p *Parser) expectToken(ts ...token.TokType) {
	for _, t := range ts {
		if p.curTokenIs(t) {
			p.nextToken()
			return
		}
	}

	p.error(fmt.Sprintf("expected next token to be in %v, got %v instead", ts, p.curToken))
}

func (p *Parser) curTokenIs(t token.TokType) bool {
	return p.curToken.Type == t
}

func (p *Parser) Errors() []string {
	return p.errors
}

func (p *Parser) error(err string) {
	p.errors = append(p.errors, err)
	panic(err) // TODO: remove this line when parser is stable
}

func (p *Parser) noPrefixParseFnError(t token.TokType) {
	p.error(fmt.Sprintf("no prefix parse function for %v found", t))
}

/*
Program ->
	DeclsAndExpr
*/
func (p *Parser) ParseProgram() *parse_tree.Program {
	decls, expr := p.parseDeclsAndExpr()

	program := &parse_tree.Program{
		Decls: decls,
		Expr:  expr,
	}

	// scan for unconsumed tokens
	var leftoverTokens []*token.Token
	for !p.curTokenIs(token.EOF) {
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
	«BlockLen»  DeclBlock*  ExprBlock
*/
func (p *Parser) parseDeclsAndExpr() ([]parse_tree.Block, parse_tree.Block) {
	var decls []parse_tree.Block

	numDecls := p.curToken.NumLines - 1
	p.expectToken(token.BlockLen)

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
func (p *Parser) parseDeclBlock() parse_tree.Block {
	atom := p.parseAtom()

	if p.curTokenIs(token.PatMat) {
		return p.parsePatMatBlock(atom)
	}

	return p.parseFuncDeclBlock(atom)
}

/*
PatMatBlock ->
	LValAtom  '='  ExprBlock
*/
func (p *Parser) parsePatMatBlock(atom parse_tree.Inline) parse_tree.Block {
	if !atom.IsLVal() {
		p.error(fmt.Sprintf("atom %v is not an l-value", atom))
	}

	lit := &parse_tree.PatMatBlock{Token: *p.curToken, LVal: atom}

	p.expectToken(token.PatMat)

	lit.Expr = p.parseExprBlock()

	return lit
}

/*
FuncDeclBlock ->
	ID  (LValAtom)*  FuncBlock  // sugar for currying and 'let'
*/
func (p *Parser) parseFuncDeclBlock(atom parse_tree.Inline) parse_tree.Block {
	// `atom` must be an identifier
	_, ok := atom.(*parse_tree.InlineID)
	if !ok {
		p.error(fmt.Sprintf("not a valid identifier: %v", atom))
	}

	patmat := &parse_tree.PatMatBlock{
		Token: token.Token{Type: token.PatMat, Literal: "="},
		LVal:  atom,
	}

	var args []parse_tree.Inline

	// read LValAtoms up to token.Arrow
	for !p.curTokenIs(token.Arrow) {
		arg := p.parseAtom()
		if !arg.IsLVal() {
			p.error(fmt.Sprintf("arg is not an l-value: %v", arg))
		}
		args = append(args, arg)
	}

	var innermostFunc parse_tree.Block
	if len(args) > 0 {
		innermostFunc = p.parseFuncBlock(args[len(args)-1])
	} else {
		p.error("should not be possible to get here in parseFuncDeclBlock!")
	}

	patmat.Expr = makeNestedFuncBlocks(
		innermostFunc.(*parse_tree.FuncBlock).Token,
		args[:len(args)-1],
		innermostFunc)

	return patmat
}
func makeNestedFuncBlocks(arrowToken token.Token, args []parse_tree.Inline, inner parse_tree.Block) parse_tree.Block {
	if len(args) <= 0 {
		return inner
	}

	return &parse_tree.FuncBlock{
		Token: arrowToken,
		LVal:  args[0],
		Expr:  makeNestedFuncBlocks(arrowToken, args[1:], inner),
	}
}

/*
☉ExprBlock ->
	JustExprBlock
	LetBlock
	FuncBlock
	TupleBlock
	ListBlock
*/
func (p *Parser) parseExprBlock() parse_tree.Block {
	switch p.curToken.Type {
	case token.Let:
		return p.parseLetBlock()
	case token.TBlock:
		return p.parseTupleBlock()
	case token.LBlock:
		return p.parseListBlock()
	}
	// so it's either a FuncBlock or a JustExprBlock
	atom := p.parseAtom()

	if p.curTokenIs(token.Arrow) {
		return p.parseFuncBlock(atom)
	}

	return p.parseJustExprBlock(atom)
}

/*
JustExprBlock ->
		Expr  '\n'                   // TODO: allow (by collapsing) multi-line expr
*/
func (p *Parser) parseJustExprBlock(atom parse_tree.Inline) parse_tree.Block {
	lit := &parse_tree.JustExprBlock{
		Token: token.ExprBlockToken,
		Expr:  p.parseExprRest(atom),
	}

	if !p.curTokenIs(token.EOF) {
		p.expectToken(token.NewLine)
	}

	return lit
}

/*
LetBlock ->
	'let'  '|->'  DeclsAndExpr  '<-|'
*/
func (p *Parser) parseLetBlock() parse_tree.Block {
	lit := &parse_tree.LetBlock{
		Token: *p.curToken,
	}
	p.expectToken(token.Let)
	p.expectToken(token.Indent)

	lit.Decls, lit.Expr = p.parseDeclsAndExpr()

	p.expectToken(token.Dedent)

	return lit
}

/*
FuncBlock ->
	LValAtom  '->'  ExprBlock
	LValAtom  '->'  |->'  DeclsAndExpr  '<-|'                  // sugar for 'let'
*/
func (p *Parser) parseFuncBlock(atom parse_tree.Inline) parse_tree.Block {
	p.expectToken(token.Arrow)

	if !atom.IsLVal() {
		p.error(fmt.Sprintf("atom %v is not an l-value", atom))
	}

	lit := &parse_tree.FuncBlock{
		Token: *p.curToken,
		LVal:  atom,
	}

	if p.curTokenIs(token.Indent) {
		p.expectToken(token.Indent)
		letBlock := &parse_tree.LetBlock{Token: token.Token{
			Type:    token.Let,
			Literal: "let",
		}}
		letBlock.Decls, letBlock.Expr = p.parseDeclsAndExpr()
		lit.Expr = letBlock
		p.expectToken(token.Dedent)
	} else {
		lit.Expr = p.parseExprBlock()
	}

	return lit
}

/*
TupleBlock ->
	'(*)'  |->'  ExprBlock+  '<-|'
*/
func (p *Parser) parseTupleBlock() parse_tree.Block {
	lit := &parse_tree.TupleBlock{Token: *p.curToken}
	p.expectToken(token.TBlock)
	p.expectToken(token.Indent)
	numElems := p.curToken.NumLines
	p.expectToken(token.BlockLen)

	if numElems <= 0 {
		p.error(fmt.Sprintf("invalide TupleBlock parsed with %v elements", numElems))
	}
	for i := 0; i < numElems; i++ {
		lit.Exprs = append(lit.Exprs, p.parseExprBlock())
	}

	p.expectToken(token.Dedent)

	return lit
}

/*
ListBlock ->
	'[*]'  |->'  ExprBlock+  (';'  ExprBlock)?  '<-|'
*/
func (p *Parser) parseListBlock() parse_tree.Block {
	lBlockToken := *p.curToken
	p.expectToken(token.LBlock)
	p.expectToken(token.Indent)
	numElems := p.curToken.NumLines
	p.expectToken(token.BlockLen)

	if numElems <= 0 {
		p.error(fmt.Sprintf("invalide ListBlock parsed with %v elements", numElems))
	}

	var heads []parse_tree.Block
	var tail parse_tree.Block

	// End before numElems-1 because the last line may be a semicolon (tail), so needs special treatment.
	for i := 0; i < numElems-1; i++ {
		heads = append(heads, p.parseExprBlock())
	}

	// Now let's check that last line
	if p.curTokenIs(token.Semicolon) {
		p.expectToken(token.Semicolon)
		tail = p.parseExprBlock()
	} else {
		heads = append(heads, p.parseExprBlock())
		// and tail stays nil
	}

	p.expectToken(token.Dedent)

	return makeNestedConsBlocks(lBlockToken, heads, tail)
}

func makeNestedConsBlocks(lBlockToken token.Token, heads []parse_tree.Block, tail parse_tree.Block) parse_tree.Block {
	if len(heads) <= 0 {
		return tail
	}

	return &parse_tree.ConsBlock{
		Token: lBlockToken,
		Head:  heads[0],
		Tail:  makeNestedConsBlocks(lBlockToken, heads[1:], tail),
	}
}

/*
☉Atom ->
	ID
	Tuple
	List
	UnopExpr
*/
func (p *Parser) parseAtom() parse_tree.Inline {
	switch p.curToken.Type {
	case token.ID:
		return p.parseID()
	case token.LParen:
		return p.parseTuple()
	case token.LBracket:
		return p.parseList()
	}

	return p.parseUnopExpr()
}

func (p *Parser) parseID() *parse_tree.InlineID {
	lit := &parse_tree.InlineID{Token: *p.curToken, Name: p.curToken.Literal}

	p.expectToken(token.ID)

	return lit
}

/*
Tuple ->
	'('  ')'
	'('  Expr  (','  Expr)*  ')'

Note that there are no 1-tuples, as those are just parenthesized
expressions. In that case, parseTuple may not return a *parse_tree.InlineTuple.
*/
func (p *Parser) parseTuple() parse_tree.Inline {
	lit := &parse_tree.InlineTuple{Token: *p.curToken}

	p.expectToken(token.LParen)

	// check for unit (empty tuple)
	if p.curTokenIs(token.RParen) {
		p.expectToken(token.RParen)
		return p.unit
	}

	var exprs []parse_tree.Inline
Loop:
	for {
		exprs = append(exprs, p.parseExpr())

		switch p.curToken.Type {
		case token.RParen:
			p.expectToken(token.RParen)
			break Loop
		case token.Comma:
			p.expectToken(token.Comma)
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
func (p *Parser) parseList() *parse_tree.InlineCons {
	lit := &parse_tree.InlineCons{Token: *p.curToken}

	p.expectToken(token.LBracket, token.Comma)

	// check for nil (empty list)
	if p.curTokenIs(token.RBracket) {
		p.expectToken(token.RBracket)
		return nil
	}

	lit.Head = p.parseExpr()

	if p.curTokenIs(token.Semicolon) {
		p.expectToken(token.Semicolon)
		lit.Tail = p.parseExpr()
		p.expectToken(token.RBracket)

		return lit
	}

	// check for end of list
	if p.curTokenIs(token.RBracket) {
		p.expectToken(token.RBracket)
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

func (p *Parser) parseUnopExpr() *parse_tree.InlineUnopExpr {
	for _, op := range unops {
		if p.curTokenIs(op) {
			lit := &parse_tree.InlineUnopExpr{Token: *p.curToken}
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
	LValAtom  '->'  Expr

ApplyExpr ->
	Expr  '@'  Expr
	Expr  '.'  Expr
	Expr  Expr
*/
func (p *Parser) parseExpr() parse_tree.Inline {
	return p.parseExpression(0)
}
func (p *Parser) parseExprRest(atom parse_tree.Inline) parse_tree.Inline {
	return p.parseExpressionRest(0, atom)
}
func (p *Parser) parseExpression(precedence int) parse_tree.Inline {
	return p.parseExpressionRest(precedence, p.parseAtom())
}
func (p *Parser) parseExpressionRest(precedence int, atom parse_tree.Inline) parse_tree.Inline {
	if precedence >= len(binopPrecs) {
		if p.curTokenIs(token.Arrow) {
			tok := p.curToken
			p.expectToken(token.Arrow)

			if !atom.IsLVal() {
				p.error(fmt.Sprintf("atom %v is not an l-value", atom))
			}

			return &parse_tree.InlineFunc{
				Token: *tok,
				LVal:  atom,
				Expr:  p.parseExpr(), // note that this resets the precedence; this is why token.Arrow is not treated as just another binop
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

	higherPrecTree := p.parseExpressionRest(precedence+1, atom)

	if binopPrecs[precedence].Contains(p.curToken.Type) {
		tok := p.curToken

		if binopPrecs[precedence].lAssoc {
			return p.parseExpressionLeft(precedence, higherPrecTree)
		}

		p.nextToken()

		samePrecTree := p.parseExpression(precedence)

		return &parse_tree.InlineBinopExpr{
			Token: *tok,
			LExpr: higherPrecTree,
			RExpr: samePrecTree,
		}
	}

	return higherPrecTree
}

func (p *Parser) parseExpressionLeft(precedence int, prevTree parse_tree.Inline) parse_tree.Inline {
	tok := p.curToken
	p.nextToken()

	higherPrecTree := p.parseExpression(precedence + 1)

	samePrecTree := &parse_tree.InlineBinopExpr{
		Token: *tok,
		LExpr: prevTree,
		RExpr: higherPrecTree,
	}

	if binopPrecs[precedence].Contains(p.curToken.Type) {
		return p.parseExpressionLeft(precedence, samePrecTree)
	}

	return samePrecTree
}

func (p *Parser) parseFuncsAndApplyExprs(prevTree parse_tree.Inline) parse_tree.Inline {
	var fn, arg parse_tree.Inline

	if p.curTokenIs(token.Dot) {
		p.expectToken(token.Dot)
		fn = p.parseAtom()
		arg = prevTree
	} else {
		fn = prevTree
		arg = p.parseAtom()
	}

	applyExpr := &parse_tree.InlineBinopExpr{
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
