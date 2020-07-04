package parser

import (
	"crisp/ast"
	"crisp/lexer"
	"crisp/token"
	"fmt"
	"testing"
)

func TestExprs(t *testing.T) {
	atomTests := []struct {
		input string
		value string
	}{
		{"5  ", "5"},
		{"true ", "true"},
		{"false", "false"},
		{"foo ", "foo"},
		{"( ) ", "()"},
		{"(bar)", "bar"},
		{"(x, y)", "(x, y)"},
		{"(5, ((true), abs))", "(5, (true, abs))"},
		{"[]", "[]"},
		{"[5]", "[5]"},
		{"[12; []]", "[12]"},
		{"[x, y]", "[x; [y]]"},
		{"[foo; bar]", "[foo; bar]"},
		{"[1; [2; [3; []]]]", "[1; [2; [3]]]"},
		{"[ 5, [[true], abs] ]", "[5; [[[true]; [abs]]]]"},

		{"-90210", "(- 90210)"},
		{"-bar  ", "(- bar)"},
		{"!true ", "(! true)"},
		{"!false", "(! false)"},
		{"!foo  ", "(! foo)"},

		{"x+1", "(x + 1)"},
		{"a+b+c", "((a + b) + c)"},
		{"a+b*c", "(a + (b * c))"},
		{"a*b+c", "((a * b) + c)"},
		{"foo * bar^2", "(foo * (bar ^ 2))"},
		{"foo * bar^2^3", "(foo * (bar ^ (2 ^ 3)))"},
		{"a&b|c", "((a & b) | c)"},
		{"a|b&c", "(a | (b & c))"},

		{"x -> 0", "(x -> 0)"},
		{"x -> x+1", "(x -> (x + 1))"},
		{"(x) -> x+1", "(x -> (x + 1))"},
		{"(a,b) -> a+b", "((a, b) -> (a + b))"},
		{"[a,b] -> a+b", "([a; [b]] -> (a + b))"},
		{"a -> b -> c -> d", "(a -> (b -> (c -> d)))"},

		{"foo bar", "(foo @ bar)"},
		{"inc(5) ", "(inc @ 5)"},
		{"(foo bar baz) (a+b^c)", "(((foo @ bar) @ baz) @ (a + (b ^ c)))"},
		{"x=99\nx", "x = 99\nx"},
	}

	for _, tt := range atomTests {
		l := lexer.New(tt.input)
		p := New(l)
		program := p.ParseProgram()
		checkParserErrors(t, p)

		progStr := program.String()

		if tt.value+"\n" != progStr {
			t.Errorf("parse test fail\n\texpected:\n%v\n\tgot:\n%v", tt.value, progStr)
		}
	}
}

func TestUnopExprs(t *testing.T) {
	prefixTests := []struct {
		input string
		op    token.TokenType
		value interface{}
	}{
		{"-90210", token.MINUS, 90210},
		{"-bar  ", token.MINUS, "bar"},
		{"!true ", token.NOT, true},
		{"!false", token.NOT, false},
		{"!foo  ", token.NOT, "foo"},
	}

	for _, tt := range prefixTests {
		l := lexer.New(tt.input)
		p := New(l)
		exprAST := p.ParseProgram().Expr.(*ast.JustExprBlock).Expr
		checkParserErrors(t, p)

		switch unopAST := exprAST.(type) {
		case *ast.InlineUnopExpr:
			if tt.op != unopAST.Op() {
				t.Errorf("parsed wrong unop operator: expected %v, got %v", tt.op, unopAST.Op())
			}
			testAtom(t, unopAST.Expr, tt.value)
		default:
			t.Errorf("unrecognized unop expr %v of type %T", exprAST, exprAST)
		}
	}
}

func testAtom(t *testing.T, exp ast.Inline, expected interface{}) bool {
	switch v := expected.(type) {
	case int:
		return testInlineInt(t, exp, v)
	case bool:
		return testInlineBool(t, exp, v)
	case string:
		return testInlineID(t, exp, v)
	}
	t.Errorf("type of exp not handled. got=%T", exp)
	return false
}

func testInlineInt(t *testing.T, il ast.Inline, value int) bool {
	i, ok := il.(*ast.InlineInt)
	if !ok {
		t.Errorf("il not *ast.InlineInt. got=%T", il)
		return false
	}

	if i.Value != value {
		t.Errorf("i.Value not %d. got=%d", value, i.Value)
		return false
	}

	if i.TokenLiteral() != fmt.Sprintf("%d", value) {
		t.Errorf("i.TokenLiteral not %d. got=%s", value,
			i.TokenLiteral())
		return false
	}

	return true
}

func testInlineID(t *testing.T, exp ast.Inline, name string) bool {
	id, ok := exp.(*ast.InlineID)
	if !ok {
		t.Errorf("exp not *ast.InlineID. got=%T", exp)
		return false
	}

	if id.Name != name {
		t.Errorf("id.Name not %s. got=%s", name, id.Name)
		return false
	}

	if id.TokenLiteral() != name {
		t.Errorf("id.TokenLiteral not %s. got=%s", name, id.TokenLiteral())
		return false
	}

	return true
}

func testInlineBool(t *testing.T, exp ast.Inline, value bool) bool {
	bo, ok := exp.(*ast.InlineBool)
	if !ok {
		t.Errorf("exp not *ast.InlneBool. got=%T", exp)
		return false
	}

	if bo.Value != value {
		t.Errorf("bo.Value not %t. got=%t", value, bo.Value)
		return false
	}

	if bo.TokenLiteral() != fmt.Sprintf("%t", value) {
		t.Errorf("bo.TokenLiteral not %t. got=%s", value, bo.TokenLiteral())
		return false
	}

	return true
}

func checkParserErrors(t *testing.T, p *Parser) {
	errors := p.Errors()
	if len(errors) == 0 {
		return
	}

	t.Errorf("parser has %d errors", len(errors))
	for _, msg := range errors {
		t.Errorf("parser error: %q", msg)
	}
	t.FailNow()
}
