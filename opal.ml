(* lazy stream -------------------------------------------------------------- *)

module LazyStream = struct
  type 'a t = Cons of 'a * 'a t Lazy.t | Nil

  let of_stream stream =
    let rec next stream =
      try Cons(Stream.next stream, lazy (next stream))
      with Stream.Failure -> Nil
    in
    next stream

  let of_function f =
    let rec next f =
      match f () with
      | Some x -> Cons(x, lazy (next f))
      | None -> Nil
    in
    next f

  let of_string str = str |> Stream.of_string |> of_stream
  let of_channel ic = ic |> Stream.of_channel |> of_stream
end

(* utilities ---------------------------------------------------------------- *)

let implode l = String.concat "" (List.map (String.make 1) l)

let explode s =
  let l = ref [] in
  String.iter (fun c -> l := c :: !l) s;
  List.rev !l

let (%) f g = fun x -> g (f x)

let parse parser input =
  match parser input with
  | Some(res, _) -> Some res
  | None -> None

(* primitives --------------------------------------------------------------- *)

type 'token input = 'token LazyStream.t
type ('token, 'result) monad = ('result * 'token input) option
type ('token, 'result) parser = 'token input -> ('result * 'token input) option

let return x input = Some(x, input)

let (>>=) x f =
  fun input ->
    match x input with
    | Some(result', input') -> f result' input'
    | None -> None

let (<|>) x y =
  fun input ->
    match x input with
    | Some _ as ret -> ret
    | None -> y input

let rec scan x input =
  match x input with
  | Some(result', input') -> LazyStream.Cons(result', lazy (scan x input'))
  | None -> LazyStream.Nil

let mzero _ = None

let any = function
  | LazyStream.Cons(token, input') -> Some(token, Lazy.force input')
  | LazyStream.Nil -> None

let satisfy test =
  any >>= (fun res -> if test res then return res else mzero)

let eof x = function
  | LazyStream.Nil -> Some(x, LazyStream.Nil)
  | _ -> None

(* derived combinators ------------------------------------------------------ *)

let (=>) x f = x >>= fun r -> return (f r)
let (>>) x y = x >>= fun _ -> y
let (<<) x y = x >>= fun r -> y >>= fun _ -> return r
let (<~>) x xs = x >>= fun r -> xs >>= fun rs -> return (r :: rs)

let rec choice = function
  | [] -> mzero
  | h :: t -> h <|> choice t

let rec count n x =
  if n > 0
  then x <~> count (n - 1) x
  else return []

let between op ed x = op >> x << ed

let option default x = x <|> return default
let optional x = option () (x >> return ())

let rec skip_many x = option () (x >>= fun _ -> skip_many x)
let skip_many1 x = x >> skip_many x

let rec many x = option [] (x >>= fun r -> many x >>= fun rs -> return (r :: rs))
let many1 x = x <~> many x

let sep_by1 x sep = x <~> many (sep >> x)
let sep_by x sep = sep_by1 x sep <|> return []

let end_by1 x sep = sep_by1 x sep << sep
let end_by x sep = end_by1 x sep <|> return []

let chainl1 x op =
  let rec loop a =
    (op >>= fun f -> x >>= fun b -> loop (f a b)) <|> return a
  in
  x >>= loop
let chainl x op default = chainl1 x op <|> return default

let rec chainr1 x op =
  x >>= fun a -> (op >>= fun f -> chainr1 x op => f a) <|> return a
let chainr x op default = chainr1 x op <|> return default

(* singletons --------------------------------------------------------------- *)

let exactly x = satisfy ((=) x)
let one_of  l = satisfy (fun x -> List.mem x l)
let none_of l = satisfy (fun x -> not (List.mem x l))
let range l r = satisfy (fun x -> l <= x && x <= r)

(* char parsers ------------------------------------------------------------- *)

let space     = one_of [' '; '\t'; '\r'; '\n']
let spaces    = skip_many space
let newline   = exactly '\n'
let tab       = exactly '\t'
let upper     = range 'A' 'Z'
let lower     = range 'a' 'z'
let digit     = range '0' '9'
let letter    = lower  <|> upper
let alpha_num = letter <|> digit
let hex_digit = range 'a' 'f' <|> range 'A' 'F' <|> digit
let oct_digit = range '0' '7'

(* lex helper --------------------------------------------------------------- *)

let lexeme x = spaces >> x

let token s =
  let rec loop s i =
    if i >= String.length s
    then return s
    else exactly s.[i] >> loop s (i + 1)
  in
  lexeme (loop s 0)
