# Henlang

## Name

The name has many meanings. First of all, *henlang* is short for incompre*hen*sible *lang*uage. Next, *hen* stands for japanese 変 which means *strange*. Finally, *hen* is an animal also known as *chicken*, and [Chicken Scheme](https://call-cc.org) is my favorite Scheme implementation. Perhaps, I'll implement Henlang using it.

## Syntax and features

### Comments

From `%` till end of line.

```
% henlang does not see this line
1+(2)
```

### Operators

Henlang treats several special characters as operators. Unlike functions, they have higher precedence.

- `a←b` assigns value of `b` to `a`. `a` is unevaled.
- `'a` does not eval `a`. Just like in Scheme.
- `a:b` returns a cons pair of `a` and `b`. Just like `(cons a b)` in Scheme.
- `&f` escapes function `f` so it does not get evaluated. Thus, it can be passed as a capital argument.
- `f/a` reduces function `f` over list `a`.
- `f#a` maps function `f` to list `a`.
- `¤ name [var:val ...] { scope }` is just like [let](https://api.call-cc.org/5/doc/scheme/let) in Scheme. The `name` is optional; if `name` is passed, it can be used for recursion. If `var` is a list, return value of `val` is destructured: `¤ [[a, b]:[1, 2]] {}` binds `1` to `a` and `2` to `b`. Last expression in `scope` is returned. If there are no expressions in `scope`, `void` is returned.
- `λ arg1, arg2... { body }` is lambda. There can be no arguments or an empty `body`.

### Functions

## Reference

### Special values

Expr | Meaning
-----|--------
`⊤`|True
`⊥`|False
`∅`|Empty list

### Arithmetics

Expr | Meaning
-----|--------
`a+`| `a` as a number.
`a-`| `a` negated.
`a+(n...)` | Sum of `a` and `n`s.
`a-(n...)` | Difference of `a` and `n`s.
`a÷(n...)` | `a` divided by `n`s.
`a×(n...)` | `a` multiplied by `n`s.
`a^(n...)` | `a` to power of `n`s.
`a√` | Square root of `a`.
`a√(n)` | `n` root of `a`.

### Comparison

Expr|Meaning
----|----
`a=(b...)`|`⊤` if `a` is equal to `b...`, `⊥` otherwise.
`a<(b...)`|`⊤` if `a` is lower than `b...`, `⊥` otherwise.
`a>(b...)`|`⊤` if `a` is greater than `b...`, `⊥` otherwise.
`a≤(b...)`|`⊤` if `a` is lower than or equal to `b...`, `⊥` otherwise.
`a≥(b...)`|`⊤` if `a` is greater than or equal to `b...`, `⊥` otherwise.

If you suffix any of the functions above with `!`, they will return `⊥` or the matching arguments instead.

```
% =! is kinda useless, but there may be some applications for it.
2=(3,1+(1)) % ⊤
2=!(3,1+(1)) % 2
2=(3) % ⊥
2=!(3) % ⊥

% Others are useful for sure.
3>(2,1) % ⊤
3>!(2,1) % 3
1>(2) % ⊥
1>!(2) % ⊥
```

Also, there are shortcuts for `>(0)` and `<(0)`:

Expr|Meaning
`a+?`|`a>(0)`
`a-?`|`a<(0)` 

### Set operations

Expr | Meaning
-----|--------
`b_` | Length of `b`.
`e∈(b)` `b∋(e)` | ⊤ if `e` is in list `b`.
`e∉(b)` `b∌(e)` | ⊤ if `e` is not in list `b`. 
`a∧(b)` | ⊤ if `a` and `b`. 
`a∨(b)` | ⊤ if `a` or `b`. 
`a∩(b)` | Intersection of `a` and `b`. 
`a∪(b)` | Union of `a` and `b`.
`a∖(b)` | Difference of `a` and `b`.
`b∀(p)` | ⊤ if all elements of `b` satisfy predicate `p`.
`b∃(p)` | ⊤ if at least one element of `b` satisfies `p`.
`bE!(p)` | Find all elements of `b` that satisfy `p` and return them.

## Examples

### Find all positive numbers in a list

```
list←[-3,-2,-1,0,1,2,3]
list∃!(+?)$
% [1,2,3]
```

### Fibonacci

```
Fibonacci←λn{
  1ι∋n⇒nι;
  n-(1)Fibonacci+(n-(2)Fibonacci)
}
```

### Factorial

```
Factorial←λn{
  n=(0)⇒1;
  n-(1)Factorial(n)
}
```

