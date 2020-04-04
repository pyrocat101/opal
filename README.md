# Opal: Monadic Parser Combinators for OCaml

Opal is a minimum collection of useful parsers and combinators (~150 loc of
OCaml) that makes writing parsers easier. It is designed to be small,
self-contained, pure-functional, and only includes most essential parsers, so
that one could include single file in the project or just embed it in other
OCaml source code files.

I find myself writing lots of recursive-descent parsers from scratch in OCaml
when I was solving Hackerrank FP challenges. That's why I wrote opal: to include
it in the source code and build parsers on top of parser combinators easily.

## Example

Trivial arithmetic calculator:

~~~ocaml
open Opal

let parens = between (exactly '(') (exactly ')')
let integer = many1 digit => implode % int_of_string
let add = exactly '+' >> return ( + )
let sub = exactly '-' >> return ( - )
let mul = exactly '*' >> return ( * )
let div = exactly '/' >> return ( / )

let rec expr input = chainl1 term (add <|> sub) input
and term input = chainl1 factor (mul <|> div) input
and factor input = (parens expr <|> integer) input

let () =
  let input = LazyStream.of_channel stdin in
  match parse expr input with
  | Some ans -> Printf.printf "%d\n" ans
  | None -> print_endline "ERROR!"
~~~

For non-trivial examples, see Hackerrank challenge solutions using opal in
`examples/`.

## Documentation

The expressiveness of parser combinators are attributed to higher-order
functions and the extensive use of currying. However, due to lack of `do`
syntax, the bind operation of monad would not be as succinct as that in Haskell.

A parser monad is either `None` (indicates failure), or `Some` pair of result
and unconsumed input, where the result is a user-defined value. The input is a
lazy stream of arbitrary token type.  A parser is a function that accepts an
input and returns a parser monad. Although most parsers in opal is polymorphic
over token type and result type, some useful parsers only accepts `char` as
input token type.

Since combinators in opal are roughly based on Haskell's Parsec. The following
documentation is somehow a rip-off of Parsec's doc.

### Lazy Stream

**`type 'a LazyStream t`**

Polymorphic lazy stream type.

**`val LazyStream.of_stream : 'a Stream.t -> 'a LazyStream.t`**

Build a lazy stream from stream.

**`val LazyStream.of_function : (unit -> 'a) -> 'a LazyStream.t`**

Build a lazy stream from a function `f`. The elements in the stream is populated
by calling `f ()`.

**`val LazyStream.of_string : string -> char LazyStream.t`**

Build a char lazy stream from string.

**`val LazyStream.of_channel : in_channel -> char LazyStream.t`**

Build a char lazy stream from a input channel.

### Utilities

**`val implode : char list -> bytes`**

Implode character list into a string. Useful when used with `many`.

**`val explode : bytes -> char list`**

Explode a string into a character list.

**`val ( % ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c`**

Infix operator for left-to-right function composition. `(f % g % h) x` is
equivalent to `h (g (f x))`.

**`val parse : ('token, 'result) parser -> 'token LazyStream -> 'result option`**

`parse parser input` parses `input` with `parser`, and returns `Some result` if
succeed, or `None` on failure.

### Primitives

**`type 'token input = 'token LazyStream.t`**

**`type ('token, 'result) monad = ('result * 'token input) option`**

**`type ('token, 'result) parser = 'token input -> ('token, 'result) monad`**

A parser is a function that accepts an input and returns either `None` on
failure, or `Some (result, input')`, where `result` is user-defined value and
`input'` is unconsumed input after parsing.

**`val return : 'result -> 'token input -> ('token, 'result) monad`**

Accepts a value and an input, and returns a monad.

**`val ( >>= ) : ('t, 'r) parser -> ('r -> ('t, 'r) monad) -> ('t, 'r) parser`**

`x >>= f` returns a new parser that if parser `x` succeeds, applies function `f` 
on monad produced by `x`, and produces a new monad (a.k.a. `bind`).

**`val ( let* ) : ('t, 'r) parser -> ('r -> ('t, 'r) monad) -> ('t, 'r) parser`**

This operator is the same as `>>=` but using the `let` notation.
It is usefull to avoid ugly sequences of bindings. For exemple, `p >>= fun x -> f x` can
be rewritten `let* x = p in f x`. Combined with the `return` function, we can define complex parsers :

```ocaml
let tuple_parser =
  let* x = digit in
  let* _ = exactly ',' in
  let* y = digit in
  return (x, y)
```

**`val ( <|> ) : ('t, 'r) parser -> ('t, 'r) parser -> ('t, 'r) parser`**

Choice combinator. The parser `p <|> q` first applies `q`. If it succeeds, the
value of `p` is returned. If `p` fails, parser `q` is tried.

**`val mzero : 'a -> ('t, 'r) monad`**

A parser that always fails.

**`val any : ('t, 'r) parser`**

The parser succeeds for any token in the input. Consumes a token and returns it.

**`val satisfy : ('t, 'r) parser -> ('t -> bool) -> ('t, 'r) parser`**

The parser `satisfy test` succeeds for any token for which the supplied function
`test` returns `true`. Returns the token that is actually parsed.

**`val eof : 'a -> ('t, 'a) parser`**

The parser `eof x` succeeds if the input is exhausted. Returns value `x`.

### Derived

**`val ( => ) : ('t, 'r) parser -> ('r -> 'a) -> ('t, 'a) parser`**

Map combinator. `x => f` parses `x`. If it succeeds, returns the value of `x`
applied with `f`.

**`val ( >> ) : ('t, 'a) parser -> ('t, 'b) parser -> ('t, 'b) parser`**

Ignore-left combinator. `x >> y` parses `x` and then `y`. Returns the value
returned by `y`.

**`val ( << ) : ('t, 'a) parser -> ('t, 'b) parser -> ('t, 'a) parser`**

Ignore-right combinator. `x >> y` parses `x` and then `y`. Returns the value
returned by `x`.

**`val ( <~> ) : ('t, 'r) parser -> ('t, 'r list) parser -> 't input -> ('t, 'r list) monad`**

Cons combinator. `x <~> y` parses `x` and then `y`. Returns the value of `x`
prepended to the value of `y` (a list).

~~~ocaml
let ident = letter <~> many alpha_num
~~~

**`val choice : ('t, 'r) parser list -> ('t, 'r) parser`**

`choice ps` tries to apply the parsers in the list `ps` in order, until one of
them succeeds. Returns the value of the succeeding parser.

**`val count : int -> ('t, 'r) parser -> 't input -> ('t, 'r list) monad`**

`count n` parses `n` occurrences of `p`. If `n` is smaller or equal to zero, the
parser equals to `return []`. Returns a list of `n` values returned by `p`.

**`between : ('t, 'a) parser -> ('t, 'b) parser -> ('t, 'r) parser -> ('t, 'r) parser`**

`between open close p` parses `open`, followed by `p` and `close`. Returns the
value returned by `p`.

~~~ocaml
let braces = between (exactly '{') (exactly '}')
~~~

**`val option : 'r -> ('t, 'r) parser -> ('t, 'r) parser`**

`option default p` tries to apply parser `p`. If `p` fails, it returns the
value `default`, otherwise the value returned by `p`.

~~~ocaml
let priority = option 0 (digit => String.make 1 % int_of_string)
~~~

**`val optional : 'r -> ('t, 'r) parser -> ('t, unit) parser`**

`optional p` tries to apply parser `p`. It will parse `p` or nothing. It only
fails if `p` fails. Discard the result of `p`.

**`val skip_many : ('t, 'r) parser -> ('t, unit) parser`**

`skip_many p` applies `p` *zero or more* times, skipping its result.

~~~ocaml
let spaces = skip_many space
~~~

**`val skip_many1 : ('t, 'r) parser -> ('t, unit) parser`**

`skip_many1 p` applies `p` *one or more* times, skipping its result.

**`val many : ('t, 'r) parser -> 't input -> ('t, 'r list) monad`**

`many p` applies the parser `p` *zero or more* times. Returns a list of returned
values of `p`.

**`val many1 : ('t, 'r) parser -> 't input -> ('t, 'r list) monad`**

`many1 p` applies the parser `p` *one or more* times. Returns a list of returned
values of `p`.

**`val sep_by : ('t, 'r) parser -> ('t, 'a) parser -> 't input -> ('t, 'r list) monad`**

`sep_by p sep` parses *zero or more* occurrences of `p`, separated by `sep`.
Returns a list of values returned by `p`.

~~~ocaml
let comma_sep p = sep_by p (token ",")
~~~

**`val sep_by1 : ('t, 'r) parser -> ('t, 'a) parser -> 't input -> ('t, 'r list) monad`**

`sep_by1 p sep` parses *one or more* occurrences of `p`, separated by `sep`.
Returns a list of values returned by `p`.

**`val end_by: ('t, 'r) parser -> ('t, 'a) parser -> ('t, 'r) parser`**

`end_by p sep` parses *zero or more* ocurrences of `p`, separated and ended by
`sep`. Returns a list of values returned by `p`.

~~~ocaml
let statements = end_by statement (token ";")
~~~

**`val end_by1: ('t, 'r) parser -> ('t, 'a) parser -> ('t, 'r) parser`**

`end_by1 p sep` parses *one or more* ocurrences of `p`, separated and ended by
`sep`. Returns a list of values returned by `p`.

**`val chainl : ('t, 'r) parser -> ('t, 'r -> 'r -> 'r) parser -> 'r -> ('t, 'r) parser`**

`chainl p op default` parses *zero or more* occurrences of `p`, separated by
`op`. Returns a value obtained by a *left* associative application of all
functions by `op` to the values returned by `p`. If there are zero occurences
of `p`, the value `default` is returned.

**`val chainl1 : ('t, 'r) parser -> ('t, 'r -> 'r -> 'r) parser -> ('t, 'r) parser`**

`chainl1 p op` parses *one or more* occurrences of `p`, separate by `op`.
Returns a value obtained by a *left* associative application of all functions
returned by `op` to the values returned by `p`. This parser can be used to
eliminate left recursion which typically occurs in expression grammars. See
the arithmetic caculator example above.

**`val chainr : ('t, 'r) parser -> ('t, 'r -> 'r -> 'r) parser -> 'r -> ('t, 'r) parser`**

`chainr p op default` parses *zero or more* occurrences of `p`, separated by
`op`. Returns a value obtained by *right* associative application of all
functions returned by `op` to the values returned by `p`. If there are no
occurrences of `p`, the value `x` is returned.

**`val chainr1 : ('t, 'r) parser -> ('t, 'r -> 'r -> 'r) parser -> ('t, 'r) parser`**

`chainr p op` parses *one or more* occurrences of `p`, separated by `op`.
Returns a value obtained by *right* associative application of all functions
returned by `op` to the values returned by `p`.

### Singletons

**`val exactly : 'r -> ('t, 'r) parser`**

`exactly x` parses a single token `x`. Returns the parsed token (i.e. `x`).

~~~ocaml
let semi_colon = exactly ';'
~~~

**`val one_of : 'r list -> ('t, 'r) parser`**

`one_of xs` succeeds if the current token is in the supplied list of tokens
`xs`. Returns the parsed token.

~~~ocaml
let vowel = one_of ['a'; 'e'; 'i'; 'o'; 'u']
~~~

**`val none_of : 'r list -> ('t, 'r) parser`**

As the dual of `one_of`, `none_of xs` succeeds if the current token *not* in
the supplied list of tokens `xs`. Returns the parsed token.

~~~ocaml
let consonant = none_of ['a'; 'e'; 'i'; 'o'; 'u']
~~~

**`val range : 'r -> 'r -> ('t, 'r) parser`**

`range low high` succeeds if the current token is in the range between `low`
and `high` (inclusive). Returns the parsed token.

### Char Parsers

**`val space = (char, char) parser`**

Parses a white space character (`'\s\t\r\n'`). Returns the parsed character.

**`val spaces = (char, unit) parser`**

Skip *zero or more* white spaces characters.

**`val newline = (char, char) parser`**

Parses a newline character (`'\n'`). Returns a newline character.

**`val tab = (char, char) parser`**

Parses a tab character (`'\t'`). Returns a tab character.

**`val upper = (char, char) parser`**

Parses an upper case letter (a character between 'A' and 'Z'). Returns the
parsed character.

**`val lower = (char, char) parser`**

Parses a lower case letter (a character between 'a' and 'z'). Returns the parsed
character.

**`val digit : (char, char) parser`**

Parses a digit. Returns the parsed character.

**`val letter = (char, char) parser`**

Parses a letter (an upper case or lower case letter). Returns the parsed
character.

**`val alpha_num = (char, char) parser`**

Parses a letter or digit. Returns the parser character.

**`val hex_digit = (char, char) parser`**

Parses a hexadecimal digit (a digit or a letter between 'a' and 'f' or 'A' and
'F'). Returns the parsed character.

**`val oct_digit = (char, char) parser`**

Parses an octal digit (a character between '0' and '7'). Returns the parsed
character.

### Lex Helper

**`val lexeme : (char, 'r) parser -> (char, 'r) parser`**

`lexeme p` first applies `skip_many space` and then parser `p`. Returns the
value returned by `p`.

**`val token : string -> char input -> (char, char list) monad`**

`token s` skips leading white spaces and parses a sequence of characters given
by string `s`. Returns the parsed character sequence as a list.

~~~ocaml
div_or_mod = token "div" <|> token "mod"
~~~
