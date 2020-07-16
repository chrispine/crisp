package ast

import (
	"crisp/lexer"
	"crisp/parser"
	"fmt"
	"testing"
)

func TestTipes(t *testing.T) {
	tests := []struct {
		expected string
		program  string
	}{
		{"([$0…] -> (($0 -> $1) -> [$1…]))", `


map[   ] _ -> [               ]
map[h;t] f -> [f(h) ; t.map(f)]

map


`},
		{"Int", "5"},
		{"Bool", "true"},
		{"(Int -> Int)", "x -> x+1"},
		{"[Int…]", "[1,2,3]"},
		{"[Ω…]", "[]"},
		{"([$0…] -> (($0 -> $1) -> [$1…]))", `


map[   ] _ -> [               ]
map[h;t] f -> [f(h) ; t.map(f)]

map


`},
		{"([$0…] -> (($0 -> $1) -> [$1…]))", `


map[   ] _ -> [               ]
map[h;t] f -> [f(h) ; t.map(f)]

num = [1].map(x -> x+1)

map


`},
	}

	for _, tt := range tests {
		expr := testExpr(t, tt.program)
		tipe := expr.FinalTipe()
		tipeStr := tipe.TipeString()

		if tipeStr != tt.expected {
			t.Fatalf("wrong type: expected %s, got %s in program:\n%s", tt.expected, tipeStr, tt.program)
		}
	}
}

func testExpr(t *testing.T, code string) Expr {
	l := lexer.New(code)
	p := parser.New(l)
	pTree, err := p.ParseProgram()
	if err != nil {
		t.Errorf(err.Error())
		t.FailNow()
	}

	tr := NewTranslator()
	program := tr.Translate(pTree)

	// check for translation errors
	errStr := ""
	errors := tr.Errors()
	if len(errors) > 0 {
		for _, msg := range errors {
			errStr += fmt.Sprintf("   translator error: %v\n", msg)
		}
		t.Errorf(errStr)
		t.FailNow()
	}

	return program
}
