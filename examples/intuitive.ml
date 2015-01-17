(*
 * Intuitive Language - Hackerrank FP Contest Challenge:
 * https://www.hackerrank.com/contests/lambda-calculi-jun14/challenges/intuitive-language
 *
 * The language is case-INSENSITIVE!
 *
 * letter ::= [a-zA-Z]
 * ident ::= <letter> ( <digit> | <letter> )*
 *
 * kwd  ::= function | is | of | assign | and | to | do | what
 * int  ::= <digit>+
 * num  ::= <int> [ / <int> ]
 * var  ::= <ident>
 * func ::= function of <int> : <exp> (, <exp>)*
 *        | <exp>
 * decl ::= <var> is <func> .
 *
 * assn ::= Assign <exp> to <var> ( AND <exp> to <var> )* !
 *
 * loop ::= do { <exp> } <assn>
 *
 * ask ::= what is ( <call> ) ( AND <call> )* ?
 *
 * exp   ::= <term> ( ( + | - ) <exp> )?
 * term  ::= <value> ( ( * | / ) <term> )?
 * value ::= [+ | -] <num> | <call> | \( exp \)
 * call  ::= <var> ( \[ <exp> \] )*
 *
 * program ::= ( decl | assn | loop | ask )*
 *
 *)

(* ----------------------------- opal.ml START ------------------------------ *)
module LazyStream = struct
  type 'a t = Cons of 'a * 'a t Lazy.t | Nil
  let of_stream stream =
    let rec next stream =
      try Cons(Stream.next stream, lazy (next stream))
      with Stream.Failure -> Nil
    in
    next stream
  let of_string str = str |> Stream.of_string |> of_stream
  let of_channel ic = ic |> Stream.of_channel |> of_stream
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
let (%) f g = fun x -> g (f x)
type 'token input = 'token LazyStream.t
type ('token, 'result) parser = 'token input -> ('result * 'token input) option
let parse parser input =
  match parser input with
  | Some(res, _) -> Some res
  | None -> None
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
let satisfy test = any >>= (fun res -> if test res then return res else mzero)
let eof x = function LazyStream.Nil -> Some(x, LazyStream.Nil) | _ -> None
let (=>) x f = x >>= fun r -> return (f r)
let (>>) x y = x >>= fun _ -> y
let (<<) x y = x >>= fun r -> y >>= fun _ -> return r
let (<~>) x xs = x >>= fun r -> xs >>= fun rs -> return (r :: rs)
let rec choice = function [] -> mzero | h :: t -> (h <|> choice t)
let rec count n x = if n > 0 then x <~> count (n - 1) x else return []
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
  let rec loop a = (op >>= fun f -> x >>= fun b -> loop (f a b)) <|> return a in
  x >>= loop
let chainl x op default = chainl1 x op <|> return default
let rec chainr1 x op =
  x >>= fun a -> (op >>= fun f -> chainr1 x op >>= f a) <|> return a
let chainr x op default = chainr1 x op <|> return default
let exactly x = satisfy ((=) x)
let one_of  l = satisfy (fun x -> List.mem x l)
let none_of l = satisfy (fun x -> not (List.mem l x))
let range l r = satisfy (fun x -> l <= x && x <= r)
let space     = one_of [' '; '\t'; '\r'; '\n']
let spaces    = skip_many space
let newline   = exactly '\n'
let tab       = exactly '\t'
let upper     = range 'A' 'Z'
let lower     = range 'a' 'z'
let digit     = range '0' '9'
let letter    = lower  <|> upper
let alpha_num = letter <|> digit
let hex_digit = range 'a' 'f' <|> range 'A' 'F'
let oct_digit = range '0' '7'
let lexeme x = spaces >> x
let token s =
  let rec loop s i =
    if i >= String.length s
    then return s
    else exactly s.[i] >> loop s (i + 1)
  in
  lexeme (loop s 0)
(* ------------------------------ opal.ml END ------------------------------- *)

(* rational number *)
type num = Ratio of int * int

let rec num_of_string s =
  if String.contains s '/' then
    let len = String.length s in
    let delim = String.index s '/' in
    let numer = String.sub s 0 delim
    and denom = String.sub s (delim + 1) (len - delim - 1) in
    Ratio (int_of_string numer, int_of_string denom) |> simplify
  else
    Ratio ((int_of_string s), 1) |> simplify

and sign x =
  if x < 0 then
    -1
  else if x = 0 then
    0
  else
    1

and string_of_num (Ratio (numer, denom)) =
  if denom = 1 then
    string_of_int numer
  else
    Format.sprintf "%s/%s" (string_of_int numer) (string_of_int denom)

and simplify (Ratio (numer, denom)) =
  if numer = 0 || denom = 0 then
    Ratio (0, 1)
  else
    let sign = (sign numer) * (sign denom) in
    let numer = abs numer in
    let denom = abs denom in
    let divisor = gcd numer denom in
    Ratio (sign * numer / divisor, denom / divisor)

and gcd a b =
  if      a = 0 then b
  else if b = 0 then a
  else if a > b then gcd b (a mod b)
  else               gcd a (b mod a)

(*
  a   c   ad + bc
  - + - = -------
  b   d     b*d
*)
let ( +/ ) (Ratio (a, b)) (Ratio (c, d)) =
  Ratio (a * d + b * c, b * d) |> simplify

(*
  a   c   ad - bc
  - - - = -------
  b   d     b*d
*)
let ( -/ ) (Ratio (a, b)) (Ratio (c, d)) =
  Ratio (a * d - b * c, b * d) |> simplify

(*
  a   c   ac
  - * - = --
  b   d   bd
*)
let ( */ ) (Ratio (a, b)) (Ratio (c, d)) =
  Ratio (a * c, b * d) |> simplify

(*
  a   c   ad
  - / - = --
  b   d   bc
*)
let ( // ) (Ratio (a, b)) (Ratio (c, d)) =
  Ratio (a * d, b * c) |> simplify

let minus_num (Ratio (a, b)) = Ratio (-a, b)

let is_integer_num (Ratio (a, b)) = b = 1

let sign_num (Ratio (a, b)) = sign a

let int_of_num (Ratio(a, b)) = a / b

(* interpreter *)

type exp = AddExp of exp * exp
         | SubExp of exp * exp
         | MulExp of exp * exp
         | DivExp of exp * exp
         | PosExp of exp
         | NegExp of exp
         | Number of num
         | Call of call

and func = exp array
and call = string * exp list
and assign = (string * exp) list

and stmt = Decl of string * func
         | Assign of assign
         | Loop of exp * assign
         | Ask of call list

type program = stmt list

type value = NumVal of num
           | FuncVal of num * num list

exception Syntax_error
exception Runtime_error

(* parser *)

let kwd s =
  let rec loop s i =
    if i >= String.length s
    then return s
    else satisfy (fun c -> Char.lowercase c = s.[i]) >> loop s (i + 1)
  in
  lexeme (loop s 0)
let comma_list x = sep_by1 x (token ",")
let parens = between (token "(") (token ")")
let bracks = between (token "[") (token "]")
let braces = between (token "{") (token "}")

let reserved = ["function"; "is"; "of"; "assign"; "and"; "to"; "do"; "what"]
let ident = (spaces >> letter <~> many alpha_num) =>
  implode % String.lowercase >>= function
  | s when List.mem s reserved -> mzero
  | s -> return s
let digits = spaces >> many1 digit => implode
let integer = digits => int_of_string
let number = digits => num_of_string

let add = token "+" >> return (fun x y -> AddExp(x, y))
let sub = token "-" >> return (fun x y -> SubExp(x, y))
let mul = token "*" >> return (fun x y -> MulExp(x, y))
let div = token "/" >> return (fun x y -> DivExp(x, y))
let pos = token "+" >> return (fun x -> PosExp(x))
let neg = token "-" >> return (fun x -> NegExp(x))

let rec expr input = (chainl1 term (add <|> sub)) input
and term input = (chainl1 value (mul <|> div)) input
and unary input = ((pos <|> neg) >>= fun op -> num_val => fun x -> op x) input
and value input = (unary <|> call_val <|> num_val <|> parens expr) input
and call_val input = (call => fun c -> Call c) input
and num_val input = (number => fun x -> Number x) input
and args input = (many (bracks expr)) input
and call input = (ident >>= fun fn -> args => fun args -> (fn, args)) input

let func_1 = expr => fun x -> [|x|]
let func_n =
  kwd "function"  >>
  kwd "of"        >>
  integer         >>= fun argc ->
  token ":"       >>
  comma_list expr >>= fun argv ->
  let args = Array.of_list argv in
  if argc + 1 <> Array.length args then mzero else return args
let func = func_1 <|> func_n
let decl =
  ident     >>= fun name ->
  kwd "is"  >>
  func      >>= fun func ->
  token "." >>
  return (Decl (name, func))

let pair = expr >>= fun rhs -> kwd "to" >> ident => fun lhs -> (lhs, rhs)
let assign_impl = kwd "assign" >> sep_by1 pair (kwd "and") << token "!"
let assign = assign_impl => fun x -> Assign(x)

let loop =
  kwd "do"    >>
  braces expr >>= fun times ->
  assign_impl >>= fun body ->
  return (Loop(times, body))

let queries = sep_by1 call (kwd "and")
let ask = kwd "what" >> kwd "is" >> queries << token "?" => fun q -> Ask(q)

let program = many (decl <|> assign <|> loop <|> ask)
let parser = parse program

(* eval *)

let rec evlis env l =
  List.iter (function
    | Decl (name, func) -> eval_decl env name func
    | Assign pairs -> eval_assign env pairs
    | Loop (times, body) -> eval_loop env times body
    | Ask queries -> eval_ask env queries
  ) l

and eval_decl env name func =
  let value = eval_func env func in
  Hashtbl.replace env name value

and eval_assign env pairs =
  List.iter (function (name, exp) ->
    let value = eval_exp env exp in
    Hashtbl.replace env name value
  ) pairs

and eval_loop env times body =
  match eval_exp env times with
    | NumVal n when is_integer_num n ->
        let times' = int_of_num n in
        for i = 1 to times' do
          eval_assign env body
        done
    | _ -> raise Runtime_error

and eval_ask env queries =
  List.iter (function query ->
    let value = eval_call env query in
    value |> string_of_value |> print_endline
  ) queries

and string_of_value v =
  match v with
    | NumVal n -> string_of_num n
    | FuncVal (k0, ki) ->
        (ki @ [k0]) |> List.map string_of_num |> String.concat ", "

and eval_func env f =
  match f with
    | [|k0|] -> eval_exp env k0
    | _ ->
        let f' = Array.map (eval_num env) f in
        let k0 = f'.(Array.length f' - 1) in
        let ki = Array.sub f' 0 (Array.length f' - 1) |> Array.to_list in
        FuncVal (k0, ki)

and eval_num env exp =
  match eval_exp env exp with
    | NumVal n -> n
    | _ -> raise Runtime_error

and binary_op f l r =
  match (l, r) with
    | (NumVal l, NumVal r) -> NumVal (f l r)
    | _ -> raise Runtime_error

and unary_op f e =
  match e with
    | NumVal e -> NumVal (f e)
    | _ -> raise Runtime_error

and ( +++ ) l r = binary_op ( +/ ) l r
and ( --- ) l r = binary_op ( -/ ) l r
and ( *** ) l r = binary_op ( */ ) l r
and ( /// ) l r = binary_op ( // ) l r

and eval_exp env exp =
  match exp with
    | AddExp (l, r) -> (eval_exp env l) +++ (eval_exp env r)
    | SubExp  (l, r) -> (eval_exp env l) --- (eval_exp env r)
    | MulExp  (l, r) -> (eval_exp env l) *** (eval_exp env r)
    | DivExp  (l, r) -> (eval_exp env l) /// (eval_exp env r)
    | PosExp e -> unary_op (function x -> x) (eval_exp env e)
    | NegExp e -> unary_op minus_num (eval_exp env e)
    | Number n -> NumVal n
    | Call c -> eval_call env c

and eval_call env (name, args) =
  let value = Hashtbl.find env name in
  match value with
    | NumVal n when args = [] -> value
    | NumVal _ -> raise Runtime_error
    | FuncVal (k0, ki) ->
        let args' = List.map (eval_num env) args in
        let f' = List.fold_left apply (k0, ki) args' in
        match f' with
          | (k0, []) -> NumVal k0
          | (k0, ki) -> FuncVal (k0, ki)

and apply (k0, ki) x =
  match ki with
    | k :: rest -> (k0 +/ x */ k, rest)
    | _ -> raise Runtime_error

and num_of_value v =
  match v with
    | NumVal n -> n
    | _ -> raise Runtime_error

and make_env () = Hashtbl.create 10

(* parse & eval *)
let rec run src =
  match parser src with
  | None -> raise Syntax_error
  | Some ast ->
      let env = make_env () in
      evlis env ast

and run_of_channel channel =
  channel |> LazyStream.of_channel |> run

and run_of_string str =
  str |> LazyStream.of_string |> run

let () = run_of_channel stdin
