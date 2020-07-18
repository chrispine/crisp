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
		{"Int", "5"},
		{"Bool", "true"},
		{"(Int -> Int)", "x -> x+1"},
		{"[Int…]", "[1,2,3]"},
		{"[$A…]", "[]"}, // TODO: should all Omegas be type errors?
		{"[Int…]", `


nil = []

nums = [1, 2, 3; nil]

nil


`},
		{"(Int -> Int)", `


dbl(x) -> x + x

dbl


`},
		{"(Int -> Int)", `


sqr(x) -> x * x

sqr


`},
		{"(Int -> Int)", `


sqr_inc(x) -> x * x + 1

sqr_inc


`},
		{"(($A, $B) -> $B)", `


(_, v) -> v


`},
		{"([$A…] -> Int)", `


len[   ] -> 0
len[_;t] -> 1 + t.len

len


`},
		{"(Int, ([$A…] -> Int))", `


len[   ] -> 0
len[_;t] -> 1 + t.len

(len[1, 2, 3] + len[{x:22, b:true}, {b:false, x:-33}], len)


`},
		{"([Int…], ((Int -> $A) -> [$A…]))", `


map[   ] _ -> [               ]
map[h;t] f -> [f(h) ; t.map(f)]

nums = [1,2,3]

(nums, map nums)


`},
		{"([$A…] -> (($A -> $B) -> [$B…]))", `


map[   ] _ -> [               ]
map[h;t] f -> [f(h) ; t.map(f)]

map


`},
		{"([Int…], ([$A…] -> (($A -> $B) -> [$B…])))", `


map[   ] _ -> [               ]
map[h;t] f -> [f(h) ; t.map(f)]

nums = [1,2,3].map(x -> x+1)

(nums, map)


`},
	}

	for _, tt := range tests {
		expr := testExpr(t, tt.program)
		tipe := expr.FinalTipe()
		tipeStr, _ := tipe.TipeString('A')

		if tipeStr != tt.expected {
			t.Errorf("wrong type:\n  expected:  %s\n  got:       %s\n  in program:\n%s",
				tt.expected, tipeStr, tt.program)
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
