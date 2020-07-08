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
		{4, "1234 % 10"},
		{3, "3 % 5"},
		{3, "8 % 5"},
		{3, "-2 % 5"},
		{3, "-7 % 5"},
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
		{5, `


add(x, y) ->
	x + y

add(2, 3)


`},
		{5, `


inc(x) ->
	add(a,b) ->
		a + b
	y = 1
	add(x,y)

inc(4)


`},
		{5, `


foo() ->
	5

foo()


`},
		{5, `


foo() -> 5

foo()


`},
		{5, `


foo = () -> 5

foo()


`},
		{5, `


foo = () ->
	5

foo()


`},
		{5, `


foo = (x) -> x+1

foo(4)


`},
		{5, `


foo = x -> x+1

foo(4)


`},
		{5, `


foo = (x -> x+1)

foo(4)


`},
		{5, `


foo = x ->
	y = 1
	x + y

foo(4)


`},
		{5, `


inc(x) ->
	real_inc(a) ->
		b = 1
		a + b
	real_inc(x)

inc(4)


`},
		{5, `


make_id() ->
	x -> x

make_id()(5)


`},
		{5, `


get_five() ->
	5

five = get_five()
five


`},
		{5, `


make_id() ->
	x -> x

id = make_id()
id(5)


`},
		{5, `


make_adder(a) ->
	b -> a + b

inc = make_adder(1)

inc(4)


`},
		{5, `


make_adder(a) ->
	adder(b) ->
		a + b
	adder

inc3 = make_adder(3)
inc3(2)


`},
		{5, `


second = (a,b) -> b

second(90210, 5)


`},
		{5, `


second = (a,b) -> b
args = (90210, 5)

second(args)


`},
		{5, `


second = (a,b) -> b
args = (*)
	90210
	5

second args


`},
		{5, `


second = [a,b] -> b
args = [*]
	90210
	5

second args


`},
		{5, `


tail = [h;t] -> t
args = [*]
	90210
	; 5

tail args


`},
		{5, `


add x y -> x + y

2 + 1.add 2


`},
		{44, `


foo(a,(b,c),d) ->
	c

yyy = (33, 44)

foo(111, yyy, 222)


`},
		{44, `


foo[a,[b,c],d] ->
	c

yyy = [33, 44]

foo[111, yyy, 222]


`},
		{22, `


head[h;t] -> h
tail[h;t] -> t

head[22,33,44,55,66]


`},
		{33, `


head[h;t] -> h
tail[h;t] -> t

head(tail[22,33,44,55,66])


`},
		{88, `


head[h;t] -> h
tail[h;t] -> t

a = [22 ; b]
b = [88, 99, 111]

head(tail a)


`},
		{88, `


head[h;t] -> h
tail[h;t] -> t

a = [22 ; b]
b = [88, 99, 111]

a.tail.head


`},
		{88, `


head[h;t] -> h
tail[h;t] -> t

a = [22 ; b]
b = [88, 99, 111]

compose(second,first) ->
	x -> x.first.second

tailhead = compose(head,tail)

a.tailhead


`},
		{123, `


id = (x -> 2*x)^0

id(123)


`},
		{11, `


double(x) -> 2*x

id = double^0

id(11)


`},
		{22, `


double(x) -> 2*x

dbl = double^1

dbl(11)


`},
		{44, `


double(x) -> 2*x

quad = double^2

quad(11)


`},
		{50, `


double(x) -> 2*x

sqr(x) -> x^2

(double*sqr) 5


`},
		{88, `


head[h;t] -> h
tail[h;t] -> t

a = [22 ; b]
b = [88, 99, 111]

tailhead = head * tail

a.tailhead


`},
		{88, `


head[h;t] -> h
tail[h;t] -> t

a = [22 ; b]
b = [88, 99, 111]

(head * tail) a


`},
		{41, `


head[h;t] -> h
tail[h;t] -> t

t4 = tail ^ 4

[91, 11, 21, 31, 41, 51, 61].t4.head


`},
		{99, `


head[h;t] -> h
tail[h;t] -> t

a = [22 ; b]
b = [88, 99, 111]

(head * tail^2) a


`},
		{1, `


head[h;_] -> h

head[1,2,3]


`},
		{5, `


five? = 5

five?


`},
		{0, `

foo(0) -> 1
foo(n) -> 0

foo(10)



`},
		{3628800, `


fact(0) -> 1
fact(n) -> n * fact(n-1)

fact(10)


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

		if intVal, ok := val.(*value.Int); !ok {
			t.Fatalf("wow, expected an int, got %v", val.Inspect())
		} else if intVal.Value != tt.expected {
			t.Fatalf("wrong int value: expected %d, got %d in program:\n%v", tt.expected, intVal, tt.program)
		}
	}
}

func testEvalIntExpr2(t *testing.T) {
	tests := []struct {
		expected int
		program  string
	}{
		{1, `


ones = [1; ones]

head[h;t] -> h
tail[h;t] -> t

ones.tail.head
head(ones)


`},
	}
	for _, tt := range tests {
		val := testEval(t, tt.program)

		if intVal, ok := val.(*value.Int); !ok {
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
		{"true == true", true},
		{"true != true", false},
		{"true != false", true},
		{"3 == 3", true},
		{"3 != 3", false},
		{"3 != 4", true},
		{"3 < 4", true},
		{"3 < 3", false},
		{"3 < 2", false},
		{"3 <= 4", true},
		{"3 <= 3", true},
		{"3 <= 2", false},
		{"3 >= 2", true},
		{"3 >= 3", true},
		{"3 >= 4", false},
		{"3 > 2", true},
		{"3 > 3", false},
		{"3 > 4", false},
	}

	for _, tt := range tests {
		val := testEval(t, tt.program)

		if boolVal, ok := val.(*value.Bool); !ok {
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
