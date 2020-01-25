# Token

This document describes how different tokens of henlang can be discriminated when lexing.

## Token types

```rust
enum Token {
  OpeningBrace, // {
  ClosingBrace, // }
  Comment(String), // %...
  OpeningBracket, // [
  ClosingBracket, // ]
  Newline, // \n
  CharacterLiteral(String), // ''
  StringLiteral(String), // ""
  OpeningParen, // (
  ClosingParen, // )
  Comma, // ,
  OpAssign, // ←
  OpQuote, // '
  OpCons, // :
  OpUnlambda, // &
  OpReduce, // /
  OpMap, // #
  OpLet, // ¤
  OpLambda, // λ
  OpThen, // ⇒
  OpElse, // ;
  Number(String),
  Symbol(String), // whatever
}
```

## Symbol rules

```
name ::= 1char_name suffix?
       | normal_char+ suffix?
special_char := // those above
1char_name := // those described in README.md
normal_char := // neither special_char nor 1char_name nor suffix
suffix := "?" | "!" | "¿" | "¡" | "⸮"
```

## Number rules

```
digit  ::= "0".."9"
number ::= digit+ ("." digit+){,1}
```

