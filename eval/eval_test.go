package eval

import (
	"crisp/ast"
	"crisp/lexer"
	"crisp/parser"
	"crisp/value"
	"testing"
)

func TestEvalIntExpr(t *testing.T) {
	tests := []struct {
		expected int
		program  string
	}{
		{5, "5"},
		{10, "10 # comment #\\$:'(%\"?@#{/$&^@`]#$~!*%= comment"},
		{-5, "-5\n"},
		{-10, "-10"},
		{10, "5 + 5 + 5 + 5 - 10"},
		{32, "2 * 2 * 2 * 2 * 2"},
		{0, "-50 + 100 + -50"},
		{20, "5 * 2 + 10"},
		{25, "5 + 2 * 10"},
		{0, "20+2*-10"},
		{60, "50 / 2 * 2 + 10"},
		{30, "2 * (5 + 10)"},
		{37, "3 * 3 * 3 + 10"},
		{37, "3 * (3 * 3) + 10"},
		{50, "(5 + 10 * 2 + 15 / 3) * 2 + -10"},
		{40, "5*2^3"},
		{40, "2^3*5"},
		{256, "2^2^3"},
		{5, `
x = 5
x`},
		{5, `
x = y+2
y = 3
x
`},
		{5, `
x = 4
let
	y = x+1
	y
`},
		{5, `


5


`},
		{9, `


(x -> x*x)(3)


`},
		{25, `


sqr = x -> x^2

sqr(5)


`},
		{25, `


sqr(x) ->
	y=x
	x*y

sqr(5)


`},
		{16, `


sqr = x ->
	y=x
	x*y

sqr(5-1)


`},
		{5, `


make_adder(n) ->
	x -> x+n

add2 = make_adder(2)

add2(3)


`},
		{1, `


fact(0) -> 1

fact(0)


`},
		// 		{5, `

		// add(x, y) ->
		// 	x + y

		// add(2, 3)

		// `},
		{5, `


5


`},
		{5, `


5


`},
		{5, `


5


`},
		{5, `


5


`},
		{5, `


5


`},
		{5, `


5


`},
		{5, `


5


`},
		{5, `


5


`},
		{5, `


5


`},
		{5, `


5


`},
		{5, `


5


`},
		{5, `


5


`},
	}

	for _, tt := range tests {
		val := testEval(t, tt.program)
		intVal, ok := val.(*value.Int)

		if !ok {
			t.Fatalf("wow, expected an int, got %v", val.Inspect())
		} else if intVal.Value != tt.expected {
			t.Fatalf("wrong int value: expected %d, got %d in program:\n%v", tt.expected, intVal, tt.program)
		}
	}
}

func TestEvalBoolExpr(t *testing.T) {
	tests := []struct {
		program  string
		expected bool
	}{
		{"true", true},
		{"false", false},
		{"!true", false},
		{"!false", true},
		{"false & (true | true)", false},
		{"(false & true) | true", true},
		{"false & true | true", true},
	}

	for _, tt := range tests {
		val := testEval(t, tt.program)
		boolVal, ok := val.(*value.Bool)

		if !ok {
			t.Fatalf("wow, expected a bool, got %v", val.Inspect())
		} else if boolVal.Value != tt.expected {
			t.Fatalf("wrong bool value: expected %v, got %v in program:\n%v", tt.expected, boolVal, tt.program)
		}
	}
}

func testEval(t *testing.T, input string) value.Value {
	l := lexer.New(input)
	p := parser.New(l)
	pTree := p.ParseProgram()

	if checkParserErrors(t, p) {
		return nil
	}

	tr := ast.NewTranslator()
	program := tr.Translate(pTree)

	checkTranslatorErrors(t, tr)

	return Eval(value.TopLevelEnv, program)
}

func checkParserErrors(t *testing.T, p *parser.Parser) bool {
	errors := p.Errors()
	if len(errors) == 0 {
		return false
	}

	t.Errorf("parser has %d errors", len(errors))
	for _, msg := range errors {
		t.Errorf("parser error: %q", msg)
	}
	t.FailNow()

	return true
}

func checkTranslatorErrors(t *testing.T, tr *ast.Translator) {
	errors := tr.Errors()
	if len(errors) == 0 {
		return
	}

	t.Errorf("translator has %d errors", len(errors))
	for _, msg := range errors {
		t.Errorf("translator error: %q", msg)
	}
	t.FailNow()
}
