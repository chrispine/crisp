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
		value interface{}
	}{
		{"5  ", 5},
		{"true ", true},
		{"false", false},
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
	}

	for _, tt := range atomTests {
		l := lexer.New(tt.input)
		p := New(l)
		atomAST := p.ParseProgram().Expr.(*ast.JustExprBlock).Expr
		checkParserErrors(t, p)

		switch ast := atomAST.(type) {
		case *ast.InlineInt:
			if tt.value != ast.Value {
				t.Errorf("parsed wrong int: expected %v, got %v", tt.value, ast.Value)
			}
		case *ast.InlineBool:
			if tt.value != ast.Value {
				t.Errorf("parsed wrong bool: expected %v, got %v", tt.value, ast.Value)
			}
		case *ast.InlineID:
			if tt.value != ast.Name {
				t.Errorf("parsed wrong ID: expected %v, got %v", tt.value, ast.Name)
			}
		case *ast.InlineTuple:
			tupleStr := ast.String()
			if tt.value != tupleStr {
				t.Errorf("parsed wrong tuple: expected %v, got %v", tt.value, tupleStr)
			}
		case *ast.InlineCons:
			consStr := ast.String()
			if tt.value != consStr {
				t.Errorf("parsed wrong cons: expected %v, got %v", tt.value, consStr)
			}
		case *ast.InlineUnopExpr:
			unopStr := ast.String()
			if tt.value != unopStr {
				t.Errorf("parsed wrong unop: expected %v, got %v", tt.value, unopStr)
			}
		case *ast.InlineBinopExpr:
			binopStr := ast.String()
			if tt.value != binopStr {
				t.Errorf("parsed wrong binop: expected %v, got %v", tt.value, binopStr)
			}
		case *ast.InlineFunc:
			funcStr := ast.String()
			if tt.value != funcStr {
				t.Errorf("parsed wrong func: expected %v, got %v", tt.value, funcStr)
			}
		default:
			t.Errorf("unrecognized expression %v of type %T", atomAST, atomAST)
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

		switch ast := exprAST.(type) {
		case *ast.InlineUnopExpr:
			if tt.op != ast.Op() {
				t.Errorf("parsed wrong unop operator: expected %v, got %v", tt.op, ast.Op())
			}
			testAtom(t, ast.Expr, tt.value)
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
