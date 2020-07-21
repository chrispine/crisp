# Crisp

Crisp is a programming language experiment, mainly just to see if I could do it.
I love programming languages and wanted to see what it would be like to make my own.

Roughly ten years later, this is the fourth or fifth iteration of Crisp.

The principles which guided my decision-making were to maximize expressiveness, beauty (or my
idea of it), and static reasoning ability. Ruby, a language I'm quite fond of, is
great on expressiveness and beauty, but terrible for static reasoning: the expression
`1 + 2` could change meaning *at runtime*.

### DISCLAIMER: This language is bad.

Don't use Crisp for anything. The terrible error messages *alone* are reason enough to
stay away. It doesn't have any kind of IO. It doesn't have strings.
You can't use it for, like, anything.

### Features
- Crisp is a purely functional language. Functions are first-class values, and there
is no mutation of any data structures.
- Crisp is a dynamically typed language. Which makes me sad. Check out my branch where I
tried to get type inference working. (I just couldn't quite get polymorphic functions to work.)
- Crisp has lazy evaluation semantics.
- Crisp is an off-side language: indentation is significant, and must be tabs, not spaces.
- Crisp in an interpreted language (until someone writes a compiler, but it's low priority
for me).
- Crisp is all about pattern-matching.

### A Meandering Intro to Crisp

Crisp source code is UTF-8. Indentation is semantically significant, and must be tabs,
not spaces (until I get a PR supporting spaces). 

The basic datatypes of Crisp are ints, floats, bools, functions, lists, tuples, and records.

```
# a List of Ints
my_ints = [5 + 0, -2^3, 225 % 10]

# a 3-tuple of type (Bool, Bool, List of Int)
a_tuple = (2 < 3, true & false, [2, 3; my_ints])

# a record of type {f: Function(Int -> Bool), num: Float}
bob_the_record = {num: 2.5, f: x -> x%2 == 0}
```

A (linked) list type denotes a variable length collection of ordered, homogeneous data.
So these are all lists of integers: `[1, 2, 3]` and `[-8]` and `[]`. The semicolon means
"this is the tail of the list", so `a_tuple` above evaluates to:
```
(true, false, [2, 3, 5, -8, 5])
```

A tuple type denotes a fixed length collection of ordered, heterogeneous data. A record type
denotes a fixed length collection of unordered, named, heterogeneous data.

All functions are first-class objects taking one argument and returning one result.
Here are some examples:

```
double = (x -> x*2)

# the equals sign is optional for function declarations,
# as are the parentheses

sqr x -> x^2

# functions can return functions
# (sometimes I parenthesize the parameter)
add_n(n) -> m -> m + n

add_2 = add_n(2)   # again, parens are optional

add_2(98)          # evaluates to 100
```

For that last example, there's a nice syntax for currying:
```
# almost like a function of two variables
add_n n m -> m + n

add_2 = add_n 2    # I said the parens were optional

add_2 98           # evaluates to 100
```

Functions always take one variable, but that variables can be a tuple, list,
or record. You access the contents through pattern-matching:

```
add pair ->
	(a, b) = pair # assigns a and b
	a + b
```

(Wow, GitHub renders those tabs as width 8, and it looks terrible. Doesn't look like
you can change it in the README.md, though. Sorry.)

In that last example, if `pair` were not a 2-tuple of type (Int, Int), it would be a
runtime error (probably with a totally incomprehensible error message).

A more idiomatic way to write that function: `add(a, b) -> a + b` That's still a
function of one parameter.

Functions can be piece-wise defined:

```
# factorial
fact(0) -> 1
fact(n) -> n * fact(n-1)

# length of a list
len[    ] -> 0       # I just like to vertically align my arrows
len[h; t] -> 1 + t.len
```

In `fact`, you see pattern-matching against `0` in the first case (which only matches
the integer 0), and `n` in the second case (which, as an unbound variable, matches with
anything.)

In `len` I used the `.` syntax, which is just a function call with the function coming
after the argument. These all mean the same thing:

- `f(x)`
- `(f x)`
- `(f)x` (I mean, you can put parens anywhere I guess.)
- `f x`
- `x.f`
- `f @ x`

Sometimes you don't want to match all of a pattern, so any identifier consisting solely of
underscores is ignored:

```
head[h; _] -> h

tail[_; t] -> t

# grab the nth item in a list
nth(n) -> head * tail^n

# infinite list of ones
ones = [1; ones]

nth(50) ones      # evaluates to 1, of course
```

With `nth` you can see that "multiplication" works on functions as well as integers.
On functions, it means function composition, so `(f*g)x` means `f(g(x))`. Exponents mean
repeated multiplication, like you'd expect. (Like I'd expect, anyway.)

Because Crisp uses lazy evaluation, it can deal with infinite lists, no problem:

```
zip_plus [h0; t0] [h1; t1] ->
	[h0 + h1; zip_plus t0 t1]

ones = [1; ones]

# all of the natural numbers: [0, 1, 2, 3...]
nats = [0; zip_plus ones nats]
```

For tuples, pattern-matching requires the pattern to have something for each component,
even if it's only `_`. So a 3-tuple literal and a 3-tuple pattern both require two
commas. For lists, we already saw how to use the semicolon (in literals and patterns)
to mean "the rest of the list".

For records, you may omit fields in pattern matching (but not in creating record
literals!), like this:
```
c = {height: 180, age: 17, birth_order: 1}

get_age{age: x, _} -> x

c.get_age          # evaluates to 17
```

More idiomatic would be `get_age{age:age, _} -> age`. Maybe I'll add syntax sugar for
that, so you can say one of these instead:

 ```
# maybe like this some day?
get_age{age:, _} -> age

# or maybe like this? idk
get_age{:age, _} -> age
```

I haven't decided. Also, I miss syntax coloring. Also also, if you actually just
wanted to get the age from that record, the even more idiomatic way:

```
c = {height: 180, age: 17, birth_order: 1}

c:age            # evaluates to 17
```

I can't believe I have a 17-year-old kid. Parenting is fabulous, though.

Pattern-matching is also fabulous, and I love it. Check these out:

```
# you should really just use ==, of course
same?(x, x) -> true
same?(x, y) -> false

3_to_5?(3|4|5) -> true
3_to_5?(_____) -> false
```

Identifiers can contain digits, even at the start of the identifier, and can
end with a `?`. It might be that in the future, Crisp will infer from a question
mark that it is either a bool, or a function that returns a bool, or a function
that returns a function that returns a bool... is there a way to say that? "Ultimately
a bool"? Anyway, you know what I mean.

So what's going on with `same?` up there? The first function piece matches against
`(x, x)`, which means it has to be a 2-tuple, and the first component is bound to
`x` (since`x` is unbound, it matches anything). The second component matches against
`x`, but now `x` is bound, so it has to be equal to whatever the first component is.
Why doesn't every language do this?

In `3_to_5?`, I use the pattern `3|4|5` meaning "match this against 3 or 4 or 5, any will
do".

*Q: If pattern-matching looks for anything bound in that scope, how can you say "I just
want a new `x` here"?*

You use the shadowing operator to shadow variables. It looks like
an S sort of:

```
x = 90210

sqr($x) -> x^2
```

In that case, sqr is defined how you'd expect, and that `x` is unrelated to the
previously bound integer; it *shadows* the other `x`.

*Q: In the `3_to_5?` example, you match either a 3 or 4 or 5, but in that piece of the
function definition, no variable is bound, so how do you know what argument was
passed in?*

Well, every function also binds the (single) argument to `arg`:

```
double_3_to_5(3|4|5) -> 2 * arg
```

Because we didn't include another piece to that function, it will double either a
3 or 4 or 5, or it will crash on any other input. (That's by design.)

### Other Crispy Bits

The `case` expression, which we use here to define a `filter` function:

```
filter[   ] _ -> []
filter[h;t] f ->
	case f(h)
		true  -> [h ; t.filter(f)]
		false ->      t.filter(f)

# let's get only the odd numbers
[1, 2, 3, 4, 5].filter(i -> i%2 == 1)     # [1, 3, 5]
```

None of those parens are necessary, but I find it more readable with them.

Case expressions, like functions, can have as many of those branches as you want. And if
none of the branches match, the program crashes, as intended. (If you didn't intend for
a hard assertion like that, put a catch-all branch at the bottom.)

In fact, `case` is just syntax sugar:

```
case expr
	foo -> this
	bar -> that
	baz -> the_other

# basically translates to
let
	$anon(foo) -> this
	$anon(bar) -> that
	$anon(baz) -> the_other
	
	anon(expr)
```

I suppose I should tell you about `let` expressions, too, but you probably already
guessed: it introduces scope (and comes with the semantically-significant indentation)
for a number (possibly zero) of declarations, followed by an expression evaluated in
that scope.

In fact, a Crisp program is effectively a big `let` expression with an implicit keyword `let`:
it's a series of declarations followed by an expression.

Declarations are all some form of pattern matching. This can be either explicitly with the
pattern-matching operator `=`, or implicitly as with function definitions. Remember
that this:

```
sqr x -> x^2
```
is actually translated to this:
```
sqr = x -> x^2
```

The block (multi-line) form of a function:

```
sqr x ->
	y = x
	x * y
```
is really just:
```
sqr = x -> let
	y = x
	x * y
```

Want more sugar? How about `module`:

```
module math
	export pi
	export sqr

	two = 2.0

	pi = 3.14159

	sqr(x) -> x ^ two

math:sqr(math:pi)

```

It's just a record:

```
math = let
	two = 2.0
	pi = 3.14159
	sqr = x -> x ^ two

	{pi:pi, sqr:sqr}

math:sqr(math:pi)
```

There are also block (multi-line) forms for list, tuple, and record literals:

```
a_list = [*]         # [1, 2, 3, 4, 5]
	1
	let
		x = 1
		x + x
	; [3, 4, 5]

a_tuple = (*)        # (true, 5)
	true
	5

point = {*}          # {x: 17, y: -2}
	x: 17
	y: -2
```

To convert between ints and floats, use the functions `i2f` and `f2i`. (Trying to add a
float and an int is a type error, so get to know your conversion functions!)

One more thing: all declarations in the same scope happen "at the same time":

```
y = x+1
x = 4

y       # y is 5
```

Cool, huh?

*Q: Why did you call it Crisp?*

Well, we named our kids after programming languages (C, Ruby, and Apl), so I felt kind
of left out and named this one after me: Chris P.

### Running Crisp

To get Crisp on your system, assuming you have Go installed and GOPATH set properly:
- clone this repo
- `$ cd crisp`
- `$ go build`
- `$ go install`

To run Crisp code, you have two options:

- `$ crisp path/to/foo.crisp` runs the program `foo.crisp`
- `$ crisp` starts the [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop)

### Crisp Internals

You really don't care unless you want to hack on the interpreter.

Packages:
- `token`: defines the tokens the lexer creates
- `lexer`: turns program text into a stream of tokens
- `parse_tree`: defines the grammatical structure of Crisp
- `parser`: turns the stream of tokens into a parse-tree
- `ast`:
  * `ast.go`: defines the abstract syntax tree
  * `translator.go`: translates parse tree to AST
- `eval`: the interpreter
- `repl`: the REPL

To run the tests: `go test ./lexer ./parser ./eval`

Special thanks to Thorsten Ball for his [excellent book](https://interpreterbook.com/)!
This interpreter is the first (and so far only) Go program I have written. It would be
in bad shape if it weren't for his guidance and structure. (Well, maybe it's in bad
shape anyway. But it would have been in much worse shape.) His book helped me approach
this with a Go mindset. Thanks, Thorsten!
