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
		program  string
		expected int
	}{
		{"5", 5},
		{"10", 10},
		{"-5", -5},
		{"-10", -10},
		{"5 + 5 + 5 + 5 - 10", 10},
		{"2 * 2 * 2 * 2 * 2", 32},
		{"-50 + 100 + -50", 0},
		{"5 * 2 + 10", 20},
		{"5 + 2 * 10", 25},
		{"20+2*-10", 0},
		{"50 / 2 * 2 + 10", 60},
		{"2 * (5 + 10)", 30},
		{"3 * 3 * 3 + 10", 37},
		{"3 * (3 * 3) + 10", 37},
		{"(5 + 10 * 2 + 15 / 3) * 2 + -10", 50},
		{"5*2^3", 40},
		{"2^3*5", 40},
		{"2^2^3", 256},
	}

	for _, tt := range tests {
		val := testEval(t, tt.program)
		intVal, ok := val.(*value.Int)

		if !ok {
			t.Fatalf("wow, expected an int, got %v", val.Inspect())
		} else if intVal.Value != tt.expected {
			t.Fatalf("wrong int value: expected %d, got %d", tt.expected, intVal)
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
			t.Fatalf("wrong bool value: expected %v, got %v", tt.expected, boolVal)
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
