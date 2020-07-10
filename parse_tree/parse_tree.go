package parse_tree

import (
	"bytes"
	"crisp/token"
)

/*

Notes:

* Productions preceded by '☉' are abstract,
  and thus do not appear in the parse tree.

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
	LValAtom  '='  ExprBlock

FuncDeclBlock ->
	ID  (LValAtom)*  FuncBlock                   // sugar for currying and 'let'

☉ExprBlock ->
	JustExprBlock
	LetBlock
	FuncBlock
	CaseBlock
	TupleBlock
	ListBlock

JustExprBlock ->
		Expr  '\n'                   // TODO: allow (by collapsing) multi-line expr

LetBlock ->
	'let'  '|->'  DeclsAndExpr  '<-|'

FuncBlock ->
	LValAtom  '->'  ExprBlock
	LValAtom  '->'  |->'  DeclsAndExpr  '<-|'                  // sugar for 'let'

CaseBlock ->
	'case'  Expr  '|->'  «BlockLen»  FuncBlock+  '<-|'

TupleBlock ->
	'(*)'  |->'  ExprBlock+  '<-|'

RecordBlock ->
	'(*)'  |->'  (ID  ':'  ExprBlock)+  '<-|'

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
	Tuple
	Record
	List
	UnopExpr

Tuple ->
	'('  ')'
	'('  Expr  (','  Expr)*  ')'

Record ->
	'{'  ID  ':'  Expr  (','  ID  ':'  Expr)*  '}'

List ->
	'['  ']'
	'['  Expr  (','  Expr)*  (';'  Expr)?  ']'

UnopExpr ->
	[-!]  Atom

BinopExpr ->
	Expr  [+-*%/^&|.@]  Expr

FuncExpr ->
	LValAtom  '->'  Expr

ApplyExpr ->
	Expr  '@'  Expr
	Expr  '.'  Expr
	Expr  Expr


// lVal elements


☉LLValAtom->
	ID
	LValTuple
	LValList

LValTuple ->
	'('  ')'
	'('  LValAtom  (','  LValAtom)*  ')'

LValList ->
	'['  ']'
	'['  LValAtom  (','  LValAtom)*  (';'  LValAtom)?  ']'

*/

// The base Node interface
type Node interface {
	TokenLiteral() string
	String() string
}

// All block nodes implement this
type Block interface {
	Node
	BlockNode()
}

// All inline nodes implement this
type Inline interface {
	Node
	IsLVal() bool
}

// neither Block nor Inline
type Program struct {
	Decls []*PatMatBlock
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
	LVal  Inline
	Expr  Block
}

func (pmb *PatMatBlock) BlockNode()           {}
func (pmb *PatMatBlock) TokenLiteral() string { return pmb.Token.Literal }
func (pmb *PatMatBlock) String() string {
	var out bytes.Buffer

	out.WriteString(pmb.LVal.String())
	out.WriteString(" = ")
	out.WriteString(pmb.Expr.String())

	return out.String()
}

type FuncBlockPiece struct {
	LVal Inline
	Expr Block
}
type FuncBlock struct {
	Token           token.Token // the token.Arrow token
	FuncBlockPieces []*FuncBlockPiece
}

func (flb *FuncBlock) BlockNode()           {}
func (flb *FuncBlock) TokenLiteral() string { return flb.Token.Literal }
func (flb *FuncBlock) String() string {
	var out bytes.Buffer

	out.WriteString(flb.FuncBlockPieces[0].LVal.String())
	out.WriteString(" -> ")
	out.WriteString(flb.FuncBlockPieces[0].Expr.String())

	return out.String()
}

type CaseBlock struct {
	Token token.Token // the token.Case token
	Expr  Inline
	Cases *FuncBlock
}

func (cb *CaseBlock) BlockNode()           {}
func (cb *CaseBlock) TokenLiteral() string { return cb.Token.Literal }
func (cb *CaseBlock) String() string {
	var out bytes.Buffer

	out.WriteString("[CASE BLOCK]")

	return out.String()
}

type LetBlock struct {
	Token token.Token // the token.Let token
	Decls []*PatMatBlock
	Expr  Block
}

func (lb *LetBlock) BlockNode()           {}
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

func (tb *TupleBlock) BlockNode()           {}
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

type RecordBlock struct {
	Token token.Token // the token.RBlock token
	Elems map[string]Block
}

func (rb *RecordBlock) BlockNode()           {}
func (rb *RecordBlock) TokenLiteral() string { return rb.Token.Literal }
func (rb *RecordBlock) String() string {
	var out bytes.Buffer

	out.WriteString("{*} {\n")

	for k, v := range rb.Elems {
		out.WriteString(k + ": " + v.String())
	}
	out.WriteString("}")

	return out.String()
}

type ConsBlock struct {
	Token token.Token // the token.LBlock token
	Head  Block
	Tail  Block
}

var NilBlock = &ConsBlock{Token: token.Token{Type: token.LBracket, Literal: "[]"}}

func (cb *ConsBlock) BlockNode()           {}
func (cb *ConsBlock) TokenLiteral() string { return cb.Token.Literal }
func (cb *ConsBlock) String() string {
	var out bytes.Buffer

	out.WriteString("[*] {\n")

	out.WriteString(cb.Head.String())

	if cb.Tail != NilBlock {
		out.WriteString("; " + cb.Tail.String())
	}

	out.WriteString("}\n")

	return out.String()
}

type JustExprBlock struct {
	Token token.Token // the token.ExprBlock token
	Expr  Inline
}

func (jeb *JustExprBlock) BlockNode()           {}
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

func (ii *InlineID) IsLVal() bool         { return true }
func (ii *InlineID) TokenLiteral() string { return ii.Token.Literal }
func (ii *InlineID) String() string       { return ii.Name }

type InlineNoMatch struct {
	Token token.Token // the token.ID token
	Name  string
}

func (ii *InlineNoMatch) IsLVal() bool         { return true }
func (ii *InlineNoMatch) TokenLiteral() string { return ii.Token.Literal }
func (ii *InlineNoMatch) String() string       { return ii.Name }

type InlineFunc struct {
	Token token.Token // the token.Arrow token
	LVal  Inline
	Expr  Inline
}

func (iFunc *InlineFunc) IsLVal() bool         { return false }
func (iFunc *InlineFunc) TokenLiteral() string { return iFunc.Token.Literal }
func (iFunc *InlineFunc) String() string {
	var out bytes.Buffer

	out.WriteString("(")
	out.WriteString(iFunc.LVal.String())
	out.WriteString(" -> ")
	out.WriteString(iFunc.Expr.String())
	out.WriteString(")")

	return out.String()
}

type InlineTuple struct {
	Token token.Token // the token.LParen token
	Exprs []Inline
}

func (it *InlineTuple) IsLVal() bool {
	for _, expr := range it.Exprs {
		if !expr.IsLVal() {
			return false
		}
	}

	return true
}
func (it *InlineTuple) TokenLiteral() string { return it.Token.Literal }
func (it *InlineTuple) String() string {
	if len(it.Exprs) == 0 {
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

type InlineRecord struct {
	Token       token.Token // the token.LParen token
	Elems       map[string]Inline
	PartialLVal bool
}

func (ir *InlineRecord) IsLVal() bool {
	for _, v := range ir.Elems {
		if !v.IsLVal() {
			return false
		}
	}

	return true
}
func (ir *InlineRecord) TokenLiteral() string { return ir.Token.Literal }
func (ir *InlineRecord) String() string {
	if len(ir.Elems) == 0 {
		return "{}"
	}

	var out bytes.Buffer

	firstElem := true

	for k, v := range ir.Elems {
		if firstElem {
			out.WriteString("{")
			firstElem = false
		} else {
			out.WriteString(", ")
		}
		out.WriteString(k + ": " + v.String())
	}
	out.WriteString("}")

	return out.String()
}

type InlineCons struct {
	Token token.Token // the token.LBracket token
	Head  Inline
	Tail  Inline
}

var InlineNil = &InlineCons{Token: token.Token{Type: token.LBracket, Literal: "[]"}}

func (ic *InlineCons) IsLVal() bool {
	if ic == InlineNil {
		return true
	}
	return ic.Head.IsLVal() && ic.Tail.IsLVal()
}
func (ic *InlineCons) TokenLiteral() string { return ic.Token.Literal }
func (ic *InlineCons) String() string {
	var out bytes.Buffer

	if ic == InlineNil {
		out.WriteString("[]")
	} else {
		out.WriteString("[")
		out.WriteString(ic.Head.String())

		if ic.Tail != InlineNil {
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

func (iue *InlineUnopExpr) IsLVal() bool {
	return iue.Token.Type == token.Shadow && iue.Expr.IsLVal()
}
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

func (ibe *InlineBinopExpr) IsLVal() bool         { return false }
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
