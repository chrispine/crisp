package ast

import (
	"crisp/parse_tree"
	"crisp/token"
	"fmt"
	"reflect"
	"sort"
)

// translates parse trees into ASTs

type Translator struct {
	trErrors []string
}

func NewTranslator() *Translator { return &Translator{} }

// A Crisp program is treated as if it is the interior of a `let` expression.
func (tr *Translator) Translate(program *parse_tree.Program) Expr {

	lb := &parse_tree.LetBlock{
		Token: token.LetToken,
		Decls: program.Decls,
		Expr:  program.Expr,
	}

	expr := tr.translateLetBlock(TopLevelExprEnv, lb, false) // this let block is not part of a function

	if len(tr.trErrors) == 0 {
		exprs := append(TopLevelExprs(), expr)
		for _, err := range CheckTipes(exprs) {
			tr.trErrors = append(tr.trErrors, err)
		}
	}

	return expr
}

func (tr *Translator) Errors() []string {
	return tr.trErrors
}

func (tr *Translator) error(err string, a ...interface{}) {
	tr.trErrors = append(tr.trErrors, fmt.Sprintf("Crisp translator error: "+err+"\n", a...))
}

func (tr *Translator) translateBlock(env *ExprEnv, blockTree parse_tree.Block) Expr {
	switch block := blockTree.(type) {
	case *parse_tree.JustExprBlock:
		return tr.translateJustExprBlock(env, block)
	case *parse_tree.LetBlock:
		return tr.translateLetBlock(env, block, false) // this let block is not part of a function
	case *parse_tree.FuncBlock:
		return tr.translateFuncBlock(env, block)
	case *parse_tree.CaseBlock:
		return tr.translateCaseBlock(env, block)
	case *parse_tree.ModuleBlock:
		return tr.translateModuleBlock(env, block)
	case *parse_tree.TupleBlock:
		return tr.translateTupleBlock(env, block)
	case *parse_tree.RecordBlock:
		return tr.translateRecordBlock(env, block)
	case *parse_tree.ConsBlock:
		return tr.translateConsBlock(env, block)
	case *TupleDestructureBlock:
		return tr.translateTupleDestructureBlock(env, block)
	case *RecordDestructureBlock:
		return tr.translateRecordDestructureBlock(env, block)
	case *ConsDestructureBlock:
		return tr.translateConsDestructureBlock(env, block)
	}

	tr.error("unhandled block %v of type %T", blockTree, blockTree)
	return nil
}

func (tr *Translator) translateInline(env *ExprEnv, inlineTree parse_tree.Inline) Expr {
	switch inline := inlineTree.(type) {
	case *parse_tree.InlineID:
		lookup := env.LookupIndices(inline.Name)
		if isNil(lookup) {
			tr.error("unknown identifier: " + inline.Name)
		}
		return lookup
	case *parse_tree.InlineUnopExpr:
		return &UnopExpr{
			Token: inline.Token,
			Expr:  tr.translateInline(env, inline.Expr),
		}
	case *parse_tree.InlineBinopExpr:
		if inline.Token.Type == token.Colon {
			// this is actually a record lookup
			name := inline.RExpr.(*parse_tree.InlineID).Name
			return &RecordLookupExpr{
				Name:    name,
				Names:   []string{name},
				Partial: true,
				Record:  tr.translateInline(env, inline.LExpr),
			}
		}
		if inline.Token.Type == token.Dot {
			// swap left/right and convert to '@'
			return &BinopExpr{
				Token: token.AtToken,
				LExpr: tr.translateInline(env, inline.RExpr),
				RExpr: tr.translateInline(env, inline.LExpr),
			}
		}
		if inline.Token.Type == token.NEq {
			// convert `a != b` to `!(a == b)`
			equalBinop := &BinopExpr{
				Token: token.EqualToken,
				LExpr: tr.translateInline(env, inline.LExpr),
				RExpr: tr.translateInline(env, inline.RExpr),
			}
			return &BinopExpr{
				Token: token.AtToken,
				LExpr: NotExpr,
				RExpr: equalBinop,
			}
		}
		return &BinopExpr{
			Token: inline.Token,
			LExpr: tr.translateInline(env, inline.LExpr),
			RExpr: tr.translateInline(env, inline.RExpr),
		}
	case *parse_tree.InlineFunc:
		return tr.translateInlineFunc(env, inline)
	case *parse_tree.InlineTuple:
		return tr.translateInlineTuple(env, inline)
	case *parse_tree.InlineRecord:
		return tr.translateInlineRecord(env, inline)
	case *parse_tree.InlineCons:
		return tr.translateInlineCons(env, inline)
	case *parse_tree.InlineNoMatch:
		tr.error("Invalid use of `_` outside of pattern matching.")
		return nil
	}

	tr.error("unhandled inline %v of type %T", inlineTree, inlineTree)
	return nil
}

func (tr *Translator) translateJustExprBlock(env *ExprEnv, block *parse_tree.JustExprBlock) Expr {
	return tr.translateInline(env, block.Expr)
}

type toAssert struct {
	// lhs and rhs are equal
	lhsEqual *parse_tree.InlineID
	rhsEqual parse_tree.Block
	// rhs is a cons cell
	listIsCons parse_tree.Block
	// rhs is a nil list
	listIsNil parse_tree.Block
	// one of these assertions is true
	anyOfTheseSets [][]toAssert
}

func (tr *Translator) translateLetBlock(env *ExprEnv, block *parse_tree.LetBlock, isFunc bool) *LetExpr {
	// We create two new envs: one temporary one to hold the
	// pre-translated parse tree expressions, and the one we
	// will give to the LetExpr to hold the post-translated
	// AST expressions.
	preEnv := &ParseEnv{parent: nil}
	postEnv := &ExprEnv{Parent: env}

	if isFunc {
		// Bindings[0] is the argument binding.
		postEnv.Bindings = append(postEnv.Bindings, &ExprBinding{
			Name: ArgName,
			Expr: &ArgExpr{},
		})
		// We need to keep preEnv and postEnv matching, so toss
		// in a nil.
		preEnv.Bindings = append(preEnv.Bindings, nil)
	}

	le := &LetExpr{Env: postEnv}

	// For each declaration (LVal = Expr), we break it down
	// into new bindings (which go into preEnv.Bindings)
	// and new assertions (which go into assertsToTranslate).
	// Note that we cannot translate the sub-expressions until
	// the env is complete, so we save them up and do that last.
	var preAsserts []toAssert

	for _, decl := range block.Decls {
		preAsserts = tr.partitionDecl(preEnv, postEnv, preAsserts, decl.LVal, decl.Expr, false, true)
	}

	// ensure binding names are unique
	uniqueNames := map[string]bool{}

	for _, b := range postEnv.Bindings {
		if _, ok := uniqueNames[b.Name]; ok {
			panic("[internal translator error] should not be possible to get two bindings with the same name")
		}

		uniqueNames[b.Name] = true
	}

	// Now that we know the name half of all bindings in preEnv,
	// we can proceed with translation of sub-expressions to populate
	// postEnv and le.Asserts.
	for _, ta := range preAsserts {
		var newAssert Expr

		newAssert = tr.translateAssertion(postEnv, ta)

		le.Asserts = append(le.Asserts, newAssert)
	}

	for i, bindBlock := range preEnv.Bindings {
		if isFunc && i == 0 {
			// This is the arg binding which we already took care of,
			// so do nothing.
		} else {
			postEnv.Bindings[i].Expr = tr.translateBlock(postEnv, bindBlock.Expr)
		}
	}

	le.Expr = tr.translateBlock(postEnv, block.Expr)

	return le
}

func (tr *Translator) partitionDecl(
	preEnv *ParseEnv,
	postEnv *ExprEnv,
	asserts []toAssert,
	lVal parse_tree.Inline,
	rhs parse_tree.Block,
	shadowing bool,
	bindingsAllowed bool) []toAssert {

	switch lhs := lVal.(type) {
	case *parse_tree.InlineNoMatch:
		// `_ = expr`
		// do nothing
	case *parse_tree.InlineUnopExpr:
		// `$(lval) = expr`
		if lhs.Token.Type != token.Shadow {
			tr.error("illegal l-value (should not be possible to get here): %v", lhs)
		}
		asserts = tr.partitionDecl(preEnv, postEnv, asserts, lhs.Expr, rhs, true, bindingsAllowed)
	case *parse_tree.InlineBinopExpr:
		// `a | b`
		if lhs.Token.Type != token.Or {
			tr.error("illegal l-value (should not be possible to get here): %v", lhs)
		}
		if shadowing {
			tr.error("shadowing of 'or' sub-patterns is not allowed; got %v", lhs)
		}
		lAsserts := tr.partitionDecl(preEnv, postEnv, nil, lhs.LExpr, rhs, false, false)
		rAsserts := tr.partitionDecl(preEnv, postEnv, nil, lhs.RExpr, rhs, false, false)

		asserts = append(asserts, toAssert{anyOfTheseSets: [][]toAssert{lAsserts, rAsserts}})
	case *parse_tree.InlineID:
		// `x = expr`
		if !shadowing && postEnv.isDefined(lhs.Name) {
			// assert
			asserts = append(asserts, toAssert{lhsEqual: lhs, rhsEqual: rhs})
		} else {
			// binding
			if !bindingsAllowed {
				tr.error("bindings not allowed on 'or' sub-patterns; got %v", lhs)
			}
			preEnv.Bindings = append(preEnv.Bindings, &ParseBinding{Name: lhs.Name, Expr: rhs})
			postEnv.Bindings = append(postEnv.Bindings, &ExprBinding{Name: lhs.Name, Expr: nil})
			// that `Expr: nil` will be populated in just a bit with the translated `rhs`
		}
	case *parse_tree.InlineTuple:
		// `(a, b) = expr`
		// Note that we don't need to assert that `rhs` is a tuple, or what size of tuple,
		// because the type checker will flag that at compile time.
		for i, elem := range lhs.Exprs {
			td := &TupleDestructureBlock{index: i, size: len(lhs.Exprs), tuple: rhs}
			asserts = tr.partitionDecl(preEnv, postEnv, asserts, elem, td, shadowing, bindingsAllowed)
		}
	case *parse_tree.InlineRecord:
		// `{x: foo, y: bar} = expr`
		names := inlineRecordNames(lhs.Elems)

		for name, elem := range lhs.Elems {
			rd := &RecordDestructureBlock{name: name, names: names, partial: lhs.PartialLVal, record: rhs}
			asserts = tr.partitionDecl(preEnv, postEnv, asserts, elem, rd, shadowing, bindingsAllowed)
		}
	case *parse_tree.InlineCons:
		// `[a, b]` = expr
		// `[h; t]` = expr
		// `[    ]` = expr
		if lhs.IsInlineNil() {
			asserts = append(asserts, toAssert{listIsNil: rhs})
		} else {
			asserts = append(asserts, toAssert{listIsCons: rhs})

			rhsHead := &ConsDestructureBlock{true, rhs}
			asserts = tr.partitionDecl(preEnv, postEnv, asserts, lhs.Head, rhsHead, shadowing, bindingsAllowed)

			rhsTail := &ConsDestructureBlock{false, rhs}
			asserts = tr.partitionDecl(preEnv, postEnv, asserts, lhs.Tail, rhsTail, shadowing, bindingsAllowed)
		}
	default:
		tr.error("illegal l-value (should not be possible to get here): %v", lhs)
	}

	return asserts
}

func (tr *Translator) translateAssertion(env *ExprEnv, ta toAssert) Expr {
	switch {
	case !isNil(ta.listIsCons):
		return &AssertListIsConsExpr{List: tr.translateBlock(env, ta.listIsCons)}
	case !isNil(ta.listIsNil):
		return &AssertListIsNilExpr{List: tr.translateBlock(env, ta.listIsNil)}
	case !isNil(ta.anyOfTheseSets):
		var assertSets [][]Expr

		for _, set := range ta.anyOfTheseSets {
			var assertSet []Expr

			for _, assert := range set {
				assertSet = append(assertSet, tr.translateAssertion(env, assert))
			}
			assertSets = append(assertSets, assertSet)
		}
		return &AssertAnyOfTheseSets{AssertSets: assertSets}
	default:
		lExpr := env.LookupIndices(ta.lhsEqual.Name)
		if isNil(lExpr) {
			tr.error("unknown identifier: " + ta.lhsEqual.Name)
		}

		return &AssertEqualExpr{
			LExpr: lExpr,
			RExpr: tr.translateBlock(env, ta.rhsEqual),
		}
	}
}

type TupleDestructureBlock struct {
	index int
	size  int
	tuple parse_tree.Block
}

func (td *TupleDestructureBlock) BlockNode()           {}
func (td *TupleDestructureBlock) TokenLiteral() string { return "«TupleDestructureBlock»" }
func (td *TupleDestructureBlock) String() string       { return "«TupleDestructureBlock»" }

type RecordDestructureBlock struct {
	name    string
	names   []string
	partial bool
	record  parse_tree.Block
}

func (rd *RecordDestructureBlock) BlockNode()           {}
func (rd *RecordDestructureBlock) TokenLiteral() string { return "«RecordDestructureBlock»" }
func (rd *RecordDestructureBlock) String() string       { return "«RecordDestructureBlock»" }

type ConsDestructureBlock struct {
	isHead bool
	list   parse_tree.Block
}

func (td *ConsDestructureBlock) BlockNode()           {}
func (td *ConsDestructureBlock) TokenLiteral() string { return "«ConsDestructureBlock»" }
func (td *ConsDestructureBlock) String() string       { return "«ConsDestructureBlock»" }

func (tr *Translator) translateCaseBlock(env *ExprEnv, block *parse_tree.CaseBlock) *BinopExpr {
	return &BinopExpr{
		Token: token.AtToken,
		LExpr: tr.translateFuncBlock(env, block.Cases),
		RExpr: tr.translateInline(env, block.Expr),
	}
}

func (tr *Translator) translateModuleBlock(env *ExprEnv, block *parse_tree.ModuleBlock) *LetExpr {
	elems := map[string]parse_tree.Inline{}

	for _, ex := range block.Exports {
		elems[ex] = &parse_tree.InlineID{
			Token: token.Token{Type: token.ID, Literal: ex},
			Name:  ex,
		}
	}

	if len(elems) != len(block.Exports) {
		tr.error("illegal module definition: exports must be unique")
	}

	record := &parse_tree.InlineRecord{
		Token: token.Token{Type: token.LBrace, Literal: "{"},
		Elems: elems,
	}
	lit := &parse_tree.LetBlock{
		Token: token.LetToken,
		Decls: block.Decls,
		Expr: &parse_tree.JustExprBlock{
			Token: token.ExprBlockToken,
			Expr:  record,
		},
	}

	return tr.translateLetBlock(env, lit, false) // this let block is not part of a function
}

func (tr *Translator) translateFuncBlock(env *ExprEnv, block *parse_tree.FuncBlock) *FuncExpr {
	return tr.translateFunc(env, block.FuncBlockPieces)
}
func (tr *Translator) translateInlineFunc(env *ExprEnv, inline *parse_tree.InlineFunc) *FuncExpr {
	funcBlockPieces := []*parse_tree.FuncBlockPiece{{
		LVal: inline.LVal,
		Expr: &parse_tree.JustExprBlock{Expr: inline.Expr},
	}}
	return tr.translateFunc(env, funcBlockPieces)
}

func (tr *Translator) translateFunc(env *ExprEnv, funcBlockPieces []*parse_tree.FuncBlockPiece) *FuncExpr {
	var letExprs []*LetExpr

	for _, piece := range funcBlockPieces {
		letBlock := &parse_tree.LetBlock{
			Decls: []*parse_tree.PatMatBlock{{
				LVal: piece.LVal,
				Expr: &parse_tree.JustExprBlock{
					Expr: &parse_tree.InlineID{Name: ArgName},
				}},
			},
			Expr: piece.Expr,
		}
		letExpr := tr.translateLetBlock(env, letBlock, true) // this let block is part of a function

		letExprs = append(letExprs, letExpr)
	}

	return &FuncExpr{FuncPieceExprs: letExprs}
}

func (tr *Translator) translateTupleDestructureBlock(env *ExprEnv, block *TupleDestructureBlock) *TupleDestructureExpr {
	return &TupleDestructureExpr{
		Index: block.index,
		Size:  block.size,
		Tuple: tr.translateBlock(env, block.tuple),
	}
}

func (tr *Translator) translateRecordDestructureBlock(env *ExprEnv, block *RecordDestructureBlock) *RecordLookupExpr {
	return &RecordLookupExpr{
		Name:    block.name,
		Names:   block.names,
		Partial: block.partial,
		Record:  tr.translateBlock(env, block.record),
	}
}

func (tr *Translator) translateConsDestructureBlock(env *ExprEnv, block *ConsDestructureBlock) *ConsDestructureExpr {
	return &ConsDestructureExpr{
		IsHead: block.isHead,
		List:   tr.translateBlock(env, block.list),
	}
}

func (tr *Translator) translateInlineTuple(env *ExprEnv, inline *parse_tree.InlineTuple) Expr {
	if len(inline.Exprs) < 1 {
		return Unit
	}
	if len(inline.Exprs) == 1 {
		tr.error("shouldn't be possible to have an inline tuple with one element")
		return nil
	}

	var exprs []Expr

	for _, elem := range inline.Exprs {
		exprs = append(exprs, tr.translateInline(env, elem))
	}

	return &TupleExpr{Exprs: exprs}
}
func (tr *Translator) translateTupleBlock(env *ExprEnv, block *parse_tree.TupleBlock) Expr {
	if len(block.Exprs) < 1 {
		tr.error("shouldn't be possible to have a tuple block with no elements")
		return Unit
	}
	if len(block.Exprs) == 1 {
		tr.error("shouldn't be possible to have a tuple block with one element")
		return nil
	}

	var exprs []Expr

	for _, elem := range block.Exprs {
		exprs = append(exprs, tr.translateBlock(env, elem))
	}

	return &TupleExpr{Exprs: exprs}
}

func (tr *Translator) translateInlineRecord(env *ExprEnv, inline *parse_tree.InlineRecord) *RecordExpr {
	elems := map[string]Expr{}

	for k, v := range inline.Elems {
		elems[k] = tr.translateInline(env, v)
	}

	return createRecordExpr(elems, inline.PartialLVal)
}
func (tr *Translator) translateRecordBlock(env *ExprEnv, block *parse_tree.RecordBlock) *RecordExpr {
	elems := map[string]Expr{}

	for k, v := range block.Elems {
		elems[k] = tr.translateBlock(env, v)
	}

	return createRecordExpr(elems, false)
}
func createRecordExpr(elems map[string]Expr, partial bool) *RecordExpr {
	record := &RecordExpr{Partial: partial}

	names := recordNames(elems)

	for _, name := range names {
		val := elems[name]
		record.Fields = append(record.Fields, RecordFieldExpr{Name: name, Expr: val})
	}

	return record
}
func recordNames(elems map[string]Expr) []string {
	names := make([]string, len(elems))

	i := 0
	for name := range elems {
		names[i] = name
		i++
	}

	sort.Strings(names)
	return names
}
func inlineRecordNames(elems map[string]parse_tree.Inline) []string {
	// DO NOT EDIT THIS FUNCTION. Edit the above function and copy it here.
	// If you know of a better way of dealing with Golang's lack of generics
	// and map covariance, let me know. Because this is disgusting.
	names := make([]string, len(elems))

	i := 0
	for name := range elems {
		names[i] = name
		i++
	}

	sort.Strings(names)
	return names
}

func (tr *Translator) translateInlineCons(env *ExprEnv, inline *parse_tree.InlineCons) *ConsExpr {
	if inline.IsInlineNil() {
		return &ConsExpr{}
	}

	return &ConsExpr{
		Head: tr.translateInline(env, inline.Head),
		Tail: tr.translateInline(env, inline.Tail),
	}
}
func (tr *Translator) translateConsBlock(env *ExprEnv, block *parse_tree.ConsBlock) *ConsExpr {
	if block.IsNilBlock() {
		return &ConsExpr{}
	}

	return &ConsExpr{
		Head: tr.translateBlock(env, block.Head),
		Tail: tr.translateBlock(env, block.Tail),
	}
}

// This is the name that we bind to the argument when a function is called.
var ArgName = "arg"

// By making this an "invalid" identifier name, we ensure no collisions with user code.
var IdentityName = "@identity"

func isNil(i interface{}) bool {
	return i == nil || reflect.ValueOf(i).IsNil()
}
