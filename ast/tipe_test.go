package ast

import (
	"crisp/lexer"
	"crisp/parser"
	"fmt"
	"testing"
)

/*
 * NOTE: Native (built-in) functions are not created
 * for any of these tests, and thus do not exist!
 */

func TestTipes(t *testing.T) {
	tests := []struct {
		expected string
		program  string
	}{
		{"((($A -> $B), $A) -> $B)", `

(f,x) -> f(x)

`},
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
		{"([Int…] -> ([Int…] -> [Int…]))", `


zip_plus [h0; t0] [h1; t1] ->
	[h0 + h1; zip_plus t0 t1]

zip_plus


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
		{"(([$A…] -> $A), ([$B…] -> [$B…]), ([Bool…] -> Bool))", `


head[h;_] -> h
tail[_;t] -> t


any?[   ] -> false
any?[h;t] -> h | t.any?

(head, tail, any?)


`},
		{"((($A -> $B), $A) -> $B)", `

apply(f,x) -> f(x)

apply

`},
		{"((($A -> $A), $A) -> $A)", `

apply2(f,x) -> f(f(x))

apply2

`},
		{"((($A -> Int), $A) -> Int)", `

apply_inc(f,x) -> f(x) + 1

len[   ] -> 0
len[_;t] -> 1 + t.len

foo = apply_inc(len, [true, true, false])

apply_inc

`},
		{"((($A -> Int), $A) -> Int)", `

apply_inc(f,x) -> f(x) + 1

apply_inc

`},
		{"(([$A…], ($A -> $B)) -> [$B…])", `


map([h;t], f) -> [f(h) ; map(t,f)]

map

`},
		{"([Int…], ((Int -> $A) -> [$A…]))", `


map[   ] _ -> [               ]
map[h;t] f -> [f(h) ; t.map(f)]

nums = [1,2,3]

(nums, map nums)


`},
		/*
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
			{"([$A…] -> (($A -> Bool) -> [$A…]))", `


			filter[   ] _ -> []
			filter[h;t] f ->
				case f(h)
					true  -> [h ; t.filter(f)]
					false ->      t.filter(f)

			filter


			`},
		*/
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

	tr := NewTranslator(nil)
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
