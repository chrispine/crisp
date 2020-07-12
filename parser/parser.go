package parser

import (
	"crisp/lexer"
	"crisp/parse_tree"
	"crisp/token"
	"fmt"
)

var unops = []token.TokType{
	token.Minus,  //  -
	token.Not,    //  !
	token.Shadow, //  $
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
	{lAssoc: true, ops: []token.TokType{ //  .  @
		token.Dot,
		token.At,
	}},
	{lAssoc: false, ops: []token.TokType{ //  ^  ^^
		token.Exp,
		token.DblExp,
	}},
	{lAssoc: true, ops: []token.TokType{ //  :
		token.Colon,
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
			Token: token.Token{Type: token.LParen, Literal: "()"},
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
	eoes[token.Indent] = true

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

func (p *Parser) injectToken(tok *token.Token) {
	p.curToken = tok
	p.l.Rewind()
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
func (p *Parser) parseDeclsAndExpr() ([]*parse_tree.PatMatBlock, parse_tree.Block) {
	numDecls := p.curToken.NumLines - 1
	p.expectToken(token.BlockLen)

	decls := p.parseDeclBlocks(numDecls)

	return decls, p.parseExprBlock()
}

func (p *Parser) parseDeclBlocks(numDecls int) []*parse_tree.PatMatBlock {
	var decls []*parse_tree.PatMatBlock
	var currFunc *parse_tree.FuncBlock
	var prevFunc *parse_tree.FuncBlock
	var prevDecl *parse_tree.PatMatBlock

	for i := 0; i < numDecls; i++ {
		currDecl, wasFuncDecl := p.parseDeclBlock()

		prevFunc = currFunc
		if wasFuncDecl {
			currFunc = currDecl.Expr.(*parse_tree.FuncBlock)
		} else {
			currFunc = nil
		}

		if currFunc != nil && prevFunc != nil && shouldCombine(prevDecl, currDecl) {
			// combine curr into prev
			prevFunc.FuncBlockPieces = append(prevFunc.FuncBlockPieces, currFunc.FuncBlockPieces[0])
		} else {
			decls = append(decls, currDecl)
		}

		prevDecl = currDecl
	}

	return decls
}

func shouldCombine(prevDecl *parse_tree.PatMatBlock, currDecl *parse_tree.PatMatBlock) bool {
	if prevID, ok := prevDecl.LVal.(*parse_tree.InlineID); ok {
		if currID, ok := currDecl.LVal.(*parse_tree.InlineID); ok {
			return prevID.Name == currID.Name
		}
	}
	return false
}

/*
☉DeclBlock ->
	PatMatBlock
	ModuleDeclBlock
	FuncDeclBlock
*/
func (p *Parser) parseDeclBlock() (*parse_tree.PatMatBlock, bool) {
	if p.curTokenIs(token.Module) {
		return p.parseModuleDeclBlock(), false // false: this was not a function declaration
	}

	atom := p.parseAtom()

	if p.curTokenIs(token.PatMat) {
		return p.parsePatMatBlock(atom), false // false: this was not a function declaration
	}

	return p.parseFuncDeclBlock(atom), true // true: this was a function declaration
}

/*
PatMatBlock ->
	LValAtom  '='  ExprBlock
*/
func (p *Parser) parsePatMatBlock(atom parse_tree.Inline) *parse_tree.PatMatBlock {
	if !atom.IsLVal() {
		p.error(fmt.Sprintf("atom %v is not an l-value", atom))
	}

	lit := &parse_tree.PatMatBlock{Token: *p.curToken, LVal: atom}

	p.expectToken(token.PatMat)

	lit.Expr = p.parseExprBlock()

	return lit
}

/*
ModuleDeclBlock ->
	'module'  ID  '|->'  ModuleBody  '<-|'
*/
func (p *Parser) parseModuleDeclBlock() *parse_tree.PatMatBlock {
	lit := &parse_tree.ModuleBlock{Token: *p.curToken}

	p.expectToken(token.Module)

	atom := p.parseID()
	if !atom.IsLVal() {
		p.error(fmt.Sprintf("atom %v is not an l-value", atom))
	}

	p.expectToken(token.Indent)
	p.parseModuleBody(lit)
	p.expectToken(token.Dedent)

	return &parse_tree.PatMatBlock{Token: token.PatMatToken, LVal: atom, Expr: lit}
}

/*
FuncDeclBlock ->
	ID  (LValAtom)*  FuncBlock  // sugar for currying and 'let'
*/
func (p *Parser) parseFuncDeclBlock(atom parse_tree.Inline) *parse_tree.PatMatBlock {
	// `atom` must be an identifier
	if _, ok := atom.(*parse_tree.InlineID); !ok {
		p.error(fmt.Sprintf("not a valid identifier: %v", atom))
	}

	patmat := &parse_tree.PatMatBlock{
		Token: token.PatMatToken,
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
		FuncBlockPieces: []*parse_tree.FuncBlockPiece{{
			LVal: args[0],
			Expr: makeNestedFuncBlocks(arrowToken, args[1:], inner),
		}},
	}
}

/*
☉ExprBlock ->
	JustExprBlock
	LetBlock
	FuncBlock
	CaseBlock
	TupleBlock
	ListBlock
	ModuleBlock
*/
func (p *Parser) parseExprBlock() parse_tree.Block {
	switch p.curToken.Type {
	case token.Let:
		return p.parseLetBlock()
	case token.Case:
		return p.parseCaseBlock()
	case token.TBlock:
		return p.parseTupleBlock()
	case token.RBlock:
		return p.parseRecordBlock()
	case token.LBlock:
		return p.parseListBlock()
	case token.Module:
		return p.parseModuleBlock()
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
func (p *Parser) parseJustExprBlock(atom parse_tree.Inline) *parse_tree.JustExprBlock {
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
func (p *Parser) parseLetBlock() *parse_tree.LetBlock {
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
func (p *Parser) parseFuncBlock(atom parse_tree.Inline) *parse_tree.FuncBlock {
	p.expectToken(token.Arrow)

	if !atom.IsLVal() {
		p.error(fmt.Sprintf("atom %v is not an l-value", atom))
	}

	lit := &parse_tree.FuncBlock{
		Token: *p.curToken,
		FuncBlockPieces: []*parse_tree.FuncBlockPiece{{
			LVal: atom,
		}},
	}

	if p.curTokenIs(token.Indent) {
		p.expectToken(token.Indent)
		letBlock := &parse_tree.LetBlock{Token: token.LetToken}
		letBlock.Decls, letBlock.Expr = p.parseDeclsAndExpr()
		lit.FuncBlockPieces[0].Expr = letBlock
		p.expectToken(token.Dedent)
	} else {
		lit.FuncBlockPieces[0].Expr = p.parseExprBlock()
	}

	return lit
}

/*
CaseBlock ->
	'case'  Expr  '|->'  «BlockLen»  FuncBlock+  '<-|'
*/
func (p *Parser) parseCaseBlock() *parse_tree.CaseBlock {
	lit := &parse_tree.CaseBlock{
		Token: *p.curToken,
	}
	p.expectToken(token.Case)

	lit.Expr = p.parseExpr()

	p.expectToken(token.Indent)

	numCases := p.curToken.NumLines
	p.expectToken(token.BlockLen)

	if numCases <= 0 {
		p.error("Illegal case expression: must have at least one case")
	}

	atom := p.parseAtom()
	caseFunc := p.parseFuncBlock(atom)

	// now we combine them all into one multi-piece function
	for i := 1; i < numCases; i++ {
		atom = p.parseAtom()
		funcBlock := p.parseFuncBlock(atom)

		caseFunc.FuncBlockPieces = append(caseFunc.FuncBlockPieces, funcBlock.FuncBlockPieces[0])
	}

	lit.Cases = caseFunc
	p.expectToken(token.Dedent)

	return lit
}

/*
TupleBlock ->
	'(*)'  '|->'  «BlockLen»  ExprBlock+  '<-|'
*/
func (p *Parser) parseTupleBlock() *parse_tree.TupleBlock {
	lit := &parse_tree.TupleBlock{Token: *p.curToken}
	p.expectToken(token.TBlock)
	p.expectToken(token.Indent)
	numElems := p.curToken.NumLines
	p.expectToken(token.BlockLen)

	if numElems <= 0 {
		p.error(fmt.Sprintf("invalid TupleBlock parsed with %v elements", numElems))
	}
	for i := 0; i < numElems; i++ {
		lit.Exprs = append(lit.Exprs, p.parseExprBlock())
	}

	p.expectToken(token.Dedent)

	return lit
}

/*
RecordBlock ->
	'(*)'  '|->'  «BlockLen»  (ID  ':'  ExprBlock)+  '<-|'
*/
func (p *Parser) parseRecordBlock() *parse_tree.RecordBlock {
	lit := &parse_tree.RecordBlock{Token: *p.curToken, Elems: map[string]parse_tree.Block{}}
	p.expectToken(token.RBlock)
	p.expectToken(token.Indent)
	numElems := p.curToken.NumLines
	p.expectToken(token.BlockLen)

	if numElems <= 0 {
		p.error(fmt.Sprintf("invalid RecordBlock parsed with %v elements", numElems))
	}
	for i := 0; i < numElems; i++ {
		//lit.Exprs = append(lit.Exprs, p.parseExprBlock())
		name := p.curToken.Literal
		p.expectToken(token.ID)
		p.expectToken(token.Colon)

		lit.Elems[name] = p.parseExprBlock()
	}

	if len(lit.Elems) != numElems {
		p.error("illegal record block: field names must be unique")
	}

	p.expectToken(token.Dedent)

	return lit
}

/*
ListBlock ->
	'[*]'  '|->'  «BlockLen»  ExprBlock+  (';'  ExprBlock)?  '<-|'
*/
func (p *Parser) parseListBlock() parse_tree.Block {
	lBlockToken := *p.curToken
	p.expectToken(token.LBlock)
	p.expectToken(token.Indent)
	numElems := p.curToken.NumLines
	p.expectToken(token.BlockLen)

	if numElems <= 0 {
		p.error(fmt.Sprintf("invalid ListBlock parsed with %v elements", numElems))
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
		tail = parse_tree.NilBlock
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
ModuleBlock ->
	'module'  '|->'  ModuleBody  '<-|'
*/
func (p *Parser) parseModuleBlock() *parse_tree.ModuleBlock {
	lit := &parse_tree.ModuleBlock{Token: *p.curToken}

	p.expectToken(token.Module)
	p.expectToken(token.Indent)
	p.parseModuleBody(lit)
	p.expectToken(token.Dedent)

	return lit
}

/*
ModuleBody ->
	«BlockLen»  ('export'  ID  '\n')+  DeclBlock+
*/
func (p *Parser) parseModuleBody(lit *parse_tree.ModuleBlock) {
	numLines := p.curToken.NumLines
	p.expectToken(token.BlockLen)

	for p.curTokenIs(token.Export) {
		p.expectToken(token.Export)
		lit.Exports = append(lit.Exports, p.curToken.Literal)
		p.expectToken(token.ID)
		p.expectToken(token.NewLine)
	}

	numDecls := numLines - len(lit.Exports)

	lit.Decls = p.parseDeclBlocks(numDecls)

	if len(lit.Decls) < 1 {
		p.error("invalid module definition: must include at least one declaration")
	}
}

/*
☉Atom ->
	ID
	Tuple
	Record
	List
	UnopExpr
*/
func (p *Parser) parseAtom() parse_tree.Inline {
	switch p.curToken.Type {
	case token.ID:
		return p.parseID()
	case token.NoMatch:
		return p.parseNoMatch()
	case token.LParen:
		return p.parseTuple()
	case token.LBrace:
		return p.parseRecord()
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

func (p *Parser) parseNoMatch() *parse_tree.InlineNoMatch {
	lit := &parse_tree.InlineNoMatch{Token: *p.curToken, Name: p.curToken.Literal}

	p.expectToken(token.NoMatch)

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
Record ->
	'{'  ID  ':'  Expr  (','  ID  ':'  Expr)*  '}'
*/
func (p *Parser) parseRecord() *parse_tree.InlineRecord {
	lit := &parse_tree.InlineRecord{Token: *p.curToken, Elems: map[string]parse_tree.Inline{}}

	p.expectToken(token.LBrace)

	// check for unit (empty tuple)
	if p.curTokenIs(token.RBrace) {
		p.expectToken(token.RBrace)
		p.error("Chris wants to know what use-case calls for empty records.")
		return nil
	}

	numElems := 0

Loop:
	for {
		if p.curTokenIs(token.NoMatch) {
			// this must be an l-value in a partial pattern match
			lit.PartialLVal = true
			p.expectToken(token.NoMatch)
			p.expectToken(token.RBrace)
			break
		}
		name := p.curToken.Literal
		p.expectToken(token.ID)
		p.expectToken(token.Colon)

		lit.Elems[name] = p.parseExpr()
		numElems++

		switch p.curToken.Type {
		case token.RBrace:
			p.expectToken(token.RBrace)
			break Loop
		case token.Comma:
			p.expectToken(token.Comma)
		default:
			p.error(fmt.Sprintf("malformed record: found token %v", p.curToken))
			return nil
		}
	}

	if len(lit.Elems) != numElems {
		p.error("illegal inline record: field names must be unique")
	}

	if lit.PartialLVal && !lit.IsLVal() {
		p.error("illegal inline record: must be a legal l-value")
	}

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
		return parse_tree.InlineNil
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
		lit.Tail = parse_tree.InlineNil
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
		p.injectToken(&token.AtToken)
		return atom // was: return p.parseFuncsAndApplyExprs(atom)
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
