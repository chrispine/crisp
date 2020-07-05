package parse_tree

import (
	"bytes"
	"crisp/token"
	"reflect"
)

/*

Notes:

* Productions preceded by '☉' are abstract,
  and thus do not appear in the AST.

* ListBlocks are parsed as nested ConsBlock nodes.


// compound elements


Program ->
	DeclsAndExpr

DeclsAndExpr ->
	«BlockLen»  DeclBlock*  ExprBlock


// block elements


☉DeclBlock ->
	PatMatBlock
	FuncDeclBlock

PatMatBlock ->
	LvalAtom  '='  ExprBlock

FuncDeclBlock ->
	ID  (LvalAtom)*  FuncBlock                   // sugar for currying and 'let'

☉ExprBlock ->
	JustExprBlock
	LetBlock
	FuncBlock
	TupleBlock
	ListBlock

JustExprBlock ->
		Expr  '\n'                   // TODO: allow (by collapsing) multi-line expr

LetBlock ->
	'let'  '|->'  DeclsAndExpr  '<-|'

FuncBlock ->
	LvalAtom  '->'  ExprBlock
	LvalAtom  '->'  |->'  DeclsAndExpr  '<-|'                  // sugar for 'let'

TupleBlock ->
	'(*)'  |->'  ExprBlock+  '<-|'

ListBlock ->
	'[*]'  |->'  ExprBlock+  (';'  ExprBlock)?  '<-|'


// inline elements


☉Expr ->
	Atom
	BinopExpr
	FuncExpr
	ApplyExpr

☉Atom ->
	ID
	Int
	Bool
	Tuple
	List
	UnopExpr

Tuple ->
	'('  ')'
	'('  Expr  (','  Expr)*  ')'

List ->
	'['  ']'
	'['  Expr  (','  Expr)*  (';'  Expr)?  ']'

UnopExpr ->
	[-!]  Atom

BinopExpr ->
	Expr  [+-*%/^&|.@]  Expr

FuncExpr ->
	LvalAtom  '->'  Expr

ApplyExpr ->
	Expr  '@'  Expr
	Expr  '.'  Expr
	Expr  Expr


// lval elements


☉LvalAtom ->
	ID
	Int
	Bool
	LvalTuple
	LvalList

LvalTuple ->
	'('  ')'
	'('  LvalAtom  (','  LvalAtom)*  ')'

LvalList ->
	'['  ']'
	'['  LvalAtom  (','  LvalAtom)*  (';'  LvalAtom)?  ']'

*/

// The base Node interface
type Node interface {
	TokenLiteral() string
	String() string
}

// All block nodes implement this
type Block interface {
	Node
	blockNode()
}

// All inline nodes implement this
type Inline interface {
	Node
	IsLval() bool
}

// neither Block nor Inline
type Program struct {
	Decls []Block
	Expr  Block
}

func (p *Program) TokenLiteral() string { return p.Expr.TokenLiteral() }
func (p *Program) String() string {
	var out bytes.Buffer

	for _, d := range p.Decls {
		out.WriteString(d.String())
	}

	out.WriteString(p.Expr.String())

	return out.String()
}

/*
 *   Block Nodes
 */

type PatMatBlock struct {
	Token token.Token // the token.PatMat token
	Lval  Inline
	Expr  Block
}

func (pmb *PatMatBlock) blockNode()           {}
func (pmb *PatMatBlock) TokenLiteral() string { return pmb.Token.Literal }
func (pmb *PatMatBlock) String() string {
	var out bytes.Buffer

	out.WriteString(pmb.Lval.String())
	out.WriteString(" = ")
	out.WriteString(pmb.Expr.String())

	return out.String()
}

type FuncBlock struct {
	Token token.Token // the token.Arrow token
	Lval  Inline
	Expr  Block
}

func (flb *FuncBlock) blockNode()           {}
func (flb *FuncBlock) TokenLiteral() string { return flb.Token.Literal }
func (flb *FuncBlock) String() string {
	var out bytes.Buffer

	out.WriteString(flb.Lval.String())
	out.WriteString(" -> ")
	out.WriteString(flb.Expr.String())

	return out.String()
}

type LetBlock struct {
	Token token.Token // the token.Let token
	Decls []Block
	Expr  Block
}

func (lb *LetBlock) blockNode()           {}
func (lb *LetBlock) TokenLiteral() string { return lb.Token.Literal }
func (lb *LetBlock) String() string {
	var out bytes.Buffer

	out.WriteString("let {\n")

	for _, d := range lb.Decls {
		out.WriteString(d.String())
	}

	out.WriteString(lb.Expr.String())

	out.WriteString("}\n")

	return out.String()
}

type TupleBlock struct {
	Token token.Token // the token.TBlock token
	Exprs []Block
}

func (tb *TupleBlock) blockNode()           {}
func (tb *TupleBlock) TokenLiteral() string { return tb.Token.Literal }
func (tb *TupleBlock) String() string {
	var out bytes.Buffer

	out.WriteString("(*) {\n")

	for _, e := range tb.Exprs {
		out.WriteString(e.String())
	}

	out.WriteString("}\n")

	return out.String()
}

type ConsBlock struct {
	Token token.Token // the token.LBlock token
	Head  Block
	Tail  Block
}

func (cb *ConsBlock) blockNode()           {}
func (cb *ConsBlock) TokenLiteral() string { return cb.Token.Literal }
func (cb *ConsBlock) String() string {
	var out bytes.Buffer

	out.WriteString("[*] {\n")

	out.WriteString(cb.Head.String())

	if !isNil(cb.Tail) {
		out.WriteString("; " + cb.Tail.String())
	}

	out.WriteString("}\n")

	return out.String()
}

type JustExprBlock struct {
	Token token.Token // the token.ExprBlock token
	Expr  Inline
}

func (jeb *JustExprBlock) blockNode()           {}
func (jeb *JustExprBlock) TokenLiteral() string { return jeb.Token.Literal }
func (jeb *JustExprBlock) String() string {
	var out bytes.Buffer

	out.WriteString(jeb.Expr.String())
	out.WriteString("\n")

	return out.String()
}

/*
 *   Inline Nodes
 */

type InlineID struct {
	Token token.Token // the token.ID token
	Name  string
}

func (ii *InlineID) IsLval() bool         { return true }
func (ii *InlineID) TokenLiteral() string { return ii.Token.Literal }
func (ii *InlineID) String() string       { return ii.Name }

type InlineInt struct {
	Token token.Token // the token.Int token
	Value int
}

func (ii *InlineInt) IsLval() bool         { return true }
func (ii *InlineInt) TokenLiteral() string { return ii.Token.Literal }
func (ii *InlineInt) String() string       { return ii.Token.Literal }

type InlineBool struct {
	Token token.Token // the token.True or token.False tokens
	Value bool
}

func (ib *InlineBool) IsLval() bool         { return true }
func (ib *InlineBool) TokenLiteral() string { return ib.Token.Literal }
func (ib *InlineBool) String() string       { return ib.Token.Literal }

type InlineFunc struct {
	Token token.Token // the token.Arrow token
	Lval  Inline
	Expr  Inline
}

func (iFunc *InlineFunc) IsLval() bool         { return false }
func (iFunc *InlineFunc) TokenLiteral() string { return iFunc.Token.Literal }
func (iFunc *InlineFunc) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(iFunc.Lval.String())
	out.WriteString(" -> ")
	out.WriteString(iFunc.Expr.String())
	out.WriteString(")")

	return out.String()
}

type InlineTuple struct {
	Token token.Token // the token.LParen token
	Exprs []Inline
}

func (it *InlineTuple) IsLval() bool {
	for _, expr := range it.Exprs {
		if !expr.IsLval() {
			return false
		}
	}

	return true
}
func (it *InlineTuple) TokenLiteral() string { return it.Token.Literal }
func (it *InlineTuple) String() string {
	if isNil(it) {
		return "(☣ parser error: *InlineTuple was nil ☣)"
	}

	if isNil(it.Exprs) {
		return "()"
	}

	var out bytes.Buffer

	for i, e := range it.Exprs {
		if i == 0 {
			out.WriteString("(")
		} else {
			out.WriteString(", ")
		}
		out.WriteString(e.String())
	}
	out.WriteString(")")

	return out.String()
}

type InlineCons struct {
	Token token.Token // the token.LBracket token
	Head  Inline
	Tail  Inline
}

func (ic *InlineCons) IsLval() bool         { return ic.Head.IsLval() && (isNil(ic.Tail) || ic.Tail.IsLval()) }
func (ic *InlineCons) TokenLiteral() string { return ic.Token.Literal }
func (ic *InlineCons) String() string {
	var out bytes.Buffer

	if isNil(ic) {
		out.WriteString("[]")
	} else {
		out.WriteString("[")
		out.WriteString(ic.Head.String())

		if !isNil(ic.Tail) {
			out.WriteString("; " + ic.Tail.String())
		}
		out.WriteString("]")
	}

	return out.String()
}

type InlineUnopExpr struct {
	Token token.Token // the unop token, e.g. !
	Expr  Inline
}

func (iue *InlineUnopExpr) IsLval() bool         { return false }
func (iue *InlineUnopExpr) Op() token.TokType    { return iue.Token.Type }
func (iue *InlineUnopExpr) TokenLiteral() string { return iue.Token.Literal }
func (iue *InlineUnopExpr) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(iue.Token.Literal)
	out.WriteString(" ")
	out.WriteString(iue.Expr.String())
	out.WriteString(")")

	return out.String()
}

type InlineBinopExpr struct {
	Token token.Token // the operator token, e.g. +
	LExpr Inline
	RExpr Inline
}

func (ibe *InlineBinopExpr) IsLval() bool         { return false }
func (ibe *InlineBinopExpr) Op() token.TokType    { return ibe.Token.Type }
func (ibe *InlineBinopExpr) TokenLiteral() string { return ibe.Token.Literal }
func (ibe *InlineBinopExpr) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(ibe.LExpr.String())
	out.WriteString(" " + ibe.Token.Literal + " ")
	out.WriteString(ibe.RExpr.String())
	out.WriteString(")")

	return out.String()
}

func isNil(i interface{}) bool {
	return i == nil || reflect.ValueOf(i).IsNil()
}
