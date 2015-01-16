# Opal: Monadic Parser Combinators for OCaml

Opal is a minimum collection of useful parsers and combinators (~150 loc of
OCaml) that makes writing parsers easier. It is designed to be small, self-
contained, pure-functional, and only includes most essential parsers, so that
one could include single file in the project or just embed it in other OCaml
source code files.

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
lazy stream of arbitrary token type. Although most parsers in opal is
polymorphic over token type and result type, some useful parsers only accepts
`char` as input stream token type. A parser is a function that accepts an input
and returns a parser monad.

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

**`val ( >>= ) : ('t, 'r) parser -> ('r -> ('t, 'r) monad) -> 't input -> ('t, 'r) monad`**

Accepts a parser and a function, apply the function on the monad on parser
success, and produce a new monad (a.k.a `bind`).

**`val ( <|> ) : ('t, 'r) parser -> ('t, 'r) parser -> 't input -> ('t, 'r) monad`**

Choice combinator. The parser `p <|> q` first applies `q`. If it succeeds, the
value of `p` is returned. If `p` fails, parser `q` is tried.

**`val mzero : 'a -> ('t, 'r) monad`**

A parser that always fails.

**`val any : ('t, 'r) parser -> 't input -> ('t, 'r) monad`**

The parser succeeds for any token in the input. Consumes a token and returns it.

**`val satisfy : ('t, 'r) parser -> ('t -> bool) -> 't input -> ('t, 'r) monad`**

The parser `satisfy test` succeeds for any token for which the supplied function
`test` returns `true`. Returns the token that is actually parsed.

**`val eof : 'a -> 't input -> ('t, 'a) monad`**

The parser `eof x` succeeds it the input is exhausted. Returns `x` in a monad.

### Derived

**`val ( => ) : ('t, 'r) parser -> ('r -> 'a) -> 't input -> ('t, 'a) monad`**

Map combinator. The parser `x => f` first applies `x`. If it succeeds, applies
`f` to the value of `x`.

**`val ( >> ) : ('t, 'a) parser -> ('t, 'b) parser -> 't input -> ('t, 'b) monad`**

Ignore-left combinator. The parser `x >> y` first applies `x`. If it succeeds,
discard the value of `x` and tries `y`.

**`val ( << ) : ('t, 'a) parser -> ('t, 'b) parser -> 't input -> ('t, 'a) monad`**

Ignore right combinator. The parser `x << y` tries `x` and then `y`. If they
both succeed, returns the value of `x`.

**`val ( <~> ) : ('t, 'r) parser -> ('t, 'r list) parser -> 't input -> ('t, 'r list) monad`**

Cons combinator. The parser `x <~> y` tries `x` and then `y`. If they both
succeed, returns the value of `x` prepend to the value of `y` (a list).
