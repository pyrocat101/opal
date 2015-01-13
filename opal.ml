module LazyStream = struct
  type 'a t =
    | Cons of 'a * 'a t Lazy.t
    | Nil

  let of_stream stream =
    let rec next stream =
      try Cons(Stream.next stream, lazy (next stream))
      with Stream.Failure -> Nil
    in
    next stream

  let of_string str =
    str |> Stream.of_string |> of_stream

  let of_channel ic =
    ic |> Stream.of_channel |> of_stream

  let of_function f =
    let rec next f =
      match f () with
      | Some x -> Cons(x, lazy (next f))
      | None -> Nil
    in
    next f
end

let implode l = String.concat "" (List.map (String.make 1) l)

let explode s =
  let l = ref [] in
  String.iter (fun c -> l := c :: !l) s;
  List.rev !l


type 'token input = 'token LazyStream.t
type ('token, 'result) parser = 'token input -> ('result * 'token input) option

let parse parser input =
  match parser input with
  | Some(res, _) -> Some res
  | None -> None


(* primitives *)

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

let mzero _ = None

let any = function
  | LazyStream.Cons(token, input') -> Some(token, Lazy.force input')
  | LazyStream.Nil -> None

let satisfy test =
  any >>= (fun res -> if test res then return res else mzero)

let eos x = function
  | LazyStream.Nil -> x
  | _ -> mzero


(* common combinators *)

let (^^) x f = x >>= fun r -> return (f r)
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

let end_by1 x ed = x << skip_many1 ed
let end_by x ed = x << skip_many ed

let chainl1 x op =
  let rec loop a =
    (op >>= fun f -> x >>= fun b -> loop (f a b)) <|> return a
  in
  x >>= loop
let chainl x op default = chainl1 x op <|> return default

let rec chainr1 x op =
  x >>= fun a -> (op >>= fun f -> chainr1 x op >>= f a) <|> return a
let chainr x op default = chainr1 x op <|> return default


(* char parsers *)

let one_of s = satisfy (String.contains s)
let none_of s = satisfy (fun c -> not (String.contains s c))

let char c = satisfy ((=) c)

let rec char_list = function
  | [] -> return []
  | h :: t -> char h <~> char_list t

let string s = char_list (explode s)

let space     = one_of " \t\r\n"
let spaces    = skip_many space
let newline   = char 'n'
let tab       = char '\t'
let upper     = satisfy (fun c -> 'A' <= c && c <= 'Z')
let lower     = satisfy (fun c -> 'a' <= c && c <= 'z')
let digit     = satisfy (fun c -> '0' <= c && c <= '9')
let letter    = lower  <|> upper
let alpha_num = letter <|> digit
let hex_digit = satisfy (fun c -> 'a' <= c && c <= 'f' || 'A' <= c && c <= 'F')
let oct_digit = satisfy (fun c -> '0' <= c && c <= '7')


(* lexer helper *)
let lexeme x = x >>= fun res -> spaces >> return res
