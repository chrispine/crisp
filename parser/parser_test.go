package parser

import (
	"crisp/lexer"
	"crisp/parse_tree"
	"crisp/token"
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

		{"x -> 0", "x -> 0"},
		{"x -> x+1", "x -> (x + 1)"},
		{"(x) -> x+1", "x -> (x + 1)"},
		{"(a,b) -> a+b", "(a, b) -> (a + b)"},
		{"[a,b] -> a+b", "[a; [b]] -> (a + b)"},
		{"a -> b -> c -> d", "a -> b -> c -> d"},
		{"(a -> 1, a -> 2)", "((a -> 1), (a -> 2))"},

		{"foo bar", "(foo @ bar)"},
		{"inc(5) ", "(inc @ 5)"},
		{"(foo bar baz) (a+b^c)", "(((foo @ bar) @ baz) @ (a + (b ^ c)))"},
		{"x=99\nx", "x = 99\nx"},
		{"x=99\nx->x^2", "x = 99\nx -> (x ^ 2)"},

		{"(*)\n\t1\n\t2", "(*) {\n1\n2\n}"},
		{"[*]\n\t1", "[*] {\n1\n}"},
		{"[*]\n\t1\n\t2", "[*] {\n1\n; [*] {\n2\n}\n}"},
		{"[*]\n\t1\n\t; [*]\n\t\t2", "[*] {\n1\n; [*] {\n2\n}\n}"},

		{"let\n\ta=4\n\ta+1", "let {\na = 4\n(a + 1)\n}"},
		{"let\n\tlet\n\t\t5", "let {\nlet {\n5\n}\n}"},
		{"x -> let\n\ty=x\n\ty", "x -> let {\ny = x\ny\n}"},
		{"x ->\n\ty=x\n\ty", "x -> let {\ny = x\ny\n}"},

		{"sum a b ->\n\tc = a+b\n\tc\nx", "sum = a -> b -> let {\nc = (a + b)\nc\n}\nx"},
	}

	for _, tt := range atomTests {
		l := lexer.New(tt.input)
		p := New(l)
		program := p.ParseProgram()
		checkParserErrors(t, p)

		progStr := program.String()

		if tt.value+"\n" != progStr {
			t.Errorf("parse test fail\n\tinput:\n%v\n\texpected:\n%v\n\tgot:\n%v", tt.input, tt.value, progStr)
		}
	}
}

func TestUnopExprs(t *testing.T) {
	prefixTests := []struct {
		input string
		op    token.TokType
		value interface{}
	}{
		{"-90210", token.Minus, "90210"},
		{"-bar  ", token.Minus, "bar"},
		{"!true ", token.Not, "true"},
		{"!false", token.Not, "false"},
		{"!foo  ", token.Not, "foo"},
	}

	for _, tt := range prefixTests {
		l := lexer.New(tt.input)
		p := New(l)
		exprTree := p.ParseProgram().Expr.(*parse_tree.JustExprBlock).Expr
		checkParserErrors(t, p)

		switch unopTree := exprTree.(type) {
		case *parse_tree.InlineUnopExpr:
			if tt.op != unopTree.Op() {
				t.Errorf("parsed wrong unop operator: expected %v, got %v", tt.op, unopTree.Op())
			}
			testAtom(t, unopTree.Expr, tt.value)
		default:
			t.Errorf("unrecognized unop expr %v of type %T", exprTree, exprTree)
		}
	}
}

func testAtom(t *testing.T, exp parse_tree.Inline, expected interface{}) bool {
	switch v := expected.(type) {
	case string:
		return testInlineID(t, exp, v)
	}
	t.Errorf("type of exp not handled. got=%T", exp)
	return false
}

func testInlineID(t *testing.T, exp parse_tree.Inline, name string) bool {
	id, ok := exp.(*parse_tree.InlineID)
	if !ok {
		t.Errorf("exp not *parse_tree.InlineID. got=%T", exp)
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
