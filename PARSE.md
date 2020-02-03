# Parser

I've never parsed before. In this document I'll outline how it should be done.

## Literals

There's nothing complicated about them: strings, characters, numbers, unlambdas, quotes. One of them can't be followed by another one.

## Newlines

They are insignificant:

- Before/after brace.
- After open paren. Before close paren.
- Before/after comma.

Insignificance is more important than significance.

## Composition

It is not clear if this composition has capital composee unless first element has minuscule arguments (then it does not) or the first composee is a literal (then it does, it is the first composee).

```
a b → (comp (a) (b))
a(d) b → (comp (comp (a d) b))
'a b → ((decomp (comp (b))) 'a)
'x 'a b → ERROR, not expected symbol after symbol
```

Composition breaks by any operator or separator except of backslash followed by a newline.

```
a b ⇒ k d a ; z o →
(if (comp (a) (b))
  (comp (k) (d) (a))
  (comp (z) (o)))

a \
b → (comp (a) (b))
```

## Parens

If last token was a name or precedence paren block, they denote function arg list. If last token was function arg list, throw error (reason: incomprehensible). Else, they denote higher precedence.

```
a b(k, j) → (comp (a) (b k j))
(a b(k, j))(z, x) → (comp ((comp (a) (b k j)) z x))
a b(k, j) (z, x) → ERROR
```

## Blocks

Blocks can only exist nested in a let or a lambda. If you need a block similar to `begin`, choose a let.

## Assignment

Left hand must be a name

## Comma

Exists only in arg lists or lists.

## Lambda

Evals to itself!

## Comments

Gotta be stripped.

## Conditional

```
a ⇒ b → (partial-conditional a b)
x ; d → (conditional a b d) where x is a partial conditional
```

## Cons

Hands can be whatever.

