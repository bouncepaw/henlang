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
special_char := // those above
1char_name := // those described in README.md
normal_char := // neither special_char nor 1char_names nor greek_letter
               // nor suffix nor rarrow
greek_letter := "α".."κ" | "μ".."ω" | "Α" .. "Ω" // no λ
suffix := "?" | "!"
rarrow := "→"
symbol := greek_letter* normal_char* suffix*
        | greek_letter* normal_char* rarrow normal_char* greek_letter* suffix*
        | greek_letter* 1char_name suffix*
```

## Number rules

```
sign := "+" | "-"
digit := "0".."9"
number := sign{,1} digit+ ("." digit+){,1}
```

