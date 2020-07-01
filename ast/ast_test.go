package ast

import (
	"crisp/token"
	"testing"
)

func TestString(t *testing.T) {

	expr := &InlineTuple{
		Token: token.Token{Type: token.LPAREN, Literal: "("},
		Exprs: []Inline{
			&InlineID{
				Token: token.Token{Type: token.ID, Literal: "x"},
				Name:  "x",
			},
			&InlineID{
				Token: token.Token{Type: token.ID, Literal: "y"},
				Name:  "y",
			},
		},
	}

	if expr.String() != "(x, y)" {
		t.Errorf("expr.String() wrong. got=%q", expr.String())
	}
}
