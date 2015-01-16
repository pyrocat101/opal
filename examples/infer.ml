(*
 * Infer - a Hackerrank FP challenge:
 * https://www.hackerrank.com/challenges/infer
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
let end_by1 x ed = x << skip_many1 ed
let end_by x ed = x << skip_many ed
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

open Printf

type exp =
  | Var of string
  | Let of string * exp * exp
  | Fun of string list * exp
  | App of exp * exp list

exception Syntax_error
exception Runtime_error of string

(* parser *)

let reserved = ["let"; "in"; "fun"]

let initial = letter <|> exactly '_'
let subseqt = alpha_num <|> exactly '_'
let ident = (spaces >> initial <~> many subseqt) => implode >>= function
  | s when List.mem s reserved -> mzero
  | s -> return s

let parens = between (token "(") (token ")")
let bracks = between (token "[") (token "]")
let comma_list x = sep_by x (token ",")
let comma_list1 x = sep_by1 x (token ",")

let rec expr input =
  (let_expr <|> fun_expr <|> simple_expr) input
and let_expr input =
  (token "let" >>
   ident       >>= fun name ->
   token "="   >>
   expr        >>= fun value ->
   token "in"  >>
   expr        >>= fun body ->
   return (Let(name, value, body))) input
and fun_expr input =
  (token "fun" >>
   many ident  >>= fun params ->
   token "->"  >>
   expr        >>= fun body ->
   return (Fun(params, body))) input
and simple_expr input =
  let rec foldl fn =
    (args >>= fun args -> foldl (App(fn, args))) <|> return fn
  in
  (caller >>= foldl) input
and caller input =
  (parens expr <|> (ident => fun v -> Var v)) input
and args input = (parens (comma_list1 expr)) input

let parse_expr = parse expr

(* destructive-unification based implementation of algorithm W *)

type ty =
  | TConst of string
  | TApp of ty * ty list
  | TFun of ty list * ty
  | TVar of tvar ref

and tvar =
  | Poly of int
  | Bound of ty
  | Unbound of int * level

(* level: nested level of let-expression used by generalization *)
and level = int

module Env = Map.Make(String)
type env = ty Env.t

let id_counter = ref (-1)
let gen_id = fun () -> incr id_counter; !id_counter
let reset_id = fun () -> id_counter := (-1)

let fresh_var level =
  TVar(ref @@ Unbound(gen_id (), level))

let fresh_poly_var () =
  TVar(ref @@ Poly(gen_id ()))

(* printer for type *)
let rec string_of_ty (t: ty) : string =
  (* keep track of poly variables' id -> name *)
  let id_name_map = Hashtbl.create 26 in
  (* assume we only use a to z *)
  let gensym =
    let counter = ref (-1) in
    fun () -> incr counter; char_of_int (97 + !counter) |> String.make 1
  in
  let rec walk = function
    | TConst k -> k
    | TApp(t1, args) ->
        let t1 = walk t1 in
        let args = String.concat ", " (List.map walk args) in
        sprintf "%s[%s]" t1 args
    | TFun([(TFun _) as p], t1) ->
        let lhs = walk p in
        let rhs = walk t1 in
        sprintf "(%s) -> %s" lhs rhs
    | TFun([param], t1) ->
        let lhs = walk param in
        let rhs = walk t1 in
        sprintf "%s -> %s" lhs rhs
    | TFun(params, t1) ->
        let lhs = String.concat ", " @@ List.map walk params in
        let rhs = walk t1 in
        sprintf "(%s) -> %s" lhs rhs
    | TVar {contents = Poly id} ->
        begin try
          Hashtbl.find id_name_map id
        with Not_found ->
          let name = gensym () in
          Hashtbl.add id_name_map id name;
          name
        end
    | TVar {contents = Unbound(id, _)} -> "_" ^ string_of_int id
    | TVar {contents = Bound t} -> walk t
  in
    let s = walk t in
    if Hashtbl.length id_name_map > 0 then
      let vars = Hashtbl.fold (fun _ v l -> v :: l) id_name_map [] in
      let vars = Array.of_list vars in
      Array.sort compare vars;
      let vars = Array.to_list vars in
      sprintf "forall[%s] %s" (String.concat " " vars) s
    else
      s

(* generalize unbound type variable *)
let rec generalize (level: level) (t: ty) : ty =
  match t with
  (* only generalize unbound variables in let-binding expression *)
  | TVar {contents = Unbound(id, lv)} when lv > level -> TVar(ref @@ Poly id)
  | TVar {contents = Bound t'} -> generalize level t'
  | TApp(t1, args) -> TApp(generalize level t1, List.map (generalize level) args)
  | TFun(args, t1) -> TFun(List.map (generalize level) args, generalize level t1)
  | _ -> t

(* replace polymorphic type variable with unbound type variable *)
and instantiate (level: level) (t: ty) : ty =
  (* same poly var should be replaced into same unbound var. *)
  let id_var_map = Hashtbl.create 16 in
  let rec walk t = match t with
    | TVar {contents = Poly id} ->
        begin try Hashtbl.find id_var_map id
        with Not_found ->
          let var = fresh_var level in
          Hashtbl.add id_var_map id var;
          var
        end
    | TVar {contents = Bound t} -> walk t
    | TApp(t1, args) -> TApp(walk t1, List.map walk args)
    | TFun(params, t1) -> TFun(List.map walk params, walk t1)
    | _ -> t
  in
    walk t

(* destructive unification *)
let rec unify (t1: ty) (t2: ty) : unit =
  match t1, t2 with
  | _ when t1 = t2 -> ()
  (* recursive unification *)
  | TApp(x, args), TApp(x', args') -> (unify x x'; List.iter2 unify args args')
  | TFun(params, t), TFun(params', t') -> (List.iter2 unify params params'; unify t t')
  (* either is bounded, unify with bounded value instead *)
  | TVar {contents = Bound t1}, t2
  | t1, TVar {contents = Bound t2} -> unify t1 t2
  (* either one is unbounded, occurs check and update binding *)
  | TVar ({contents = Unbound(id, level)} as v), t
  | t, TVar ({contents = Unbound(id, level)} as v) -> (occurs_check id level t; v := Bound t)
  (* all other cases fail *)
  | _ -> raise @@ Runtime_error(sprintf "cannot unify %s and %s" (string_of_ty t1) (string_of_ty t2))

(* occurence check, raise exception when failed *)
and occurs_check (id: int) (level: level) (t: ty) : unit =
  let rec check = function
    | TVar {contents = Bound t} -> check t
    | TVar {contents = Unbound(id', _)} when id' = id ->
        raise @@ Runtime_error("recursive type")
    (* unify two unbounds: lift the level of the other one *)
    | TVar ({contents = Unbound(id', level')} as v) when level' > level ->
        v := Unbound(id', level)
    | TApp(t1, args) -> check t1; List.iter check args
    | TFun(args, t1) -> List.iter check args; check t1
    | _ -> ()
  in
    check t

(* W *)
let rec w (env: env) (level: level) (exp: exp) : ty =
  match exp with
  (* var *)
  | Var v ->
      begin try
        instantiate level (Env.find v env)
      with Not_found ->
        raise @@ Runtime_error("unbound type variable " ^ v)
      end
  (* abs *)
  | Fun(params, body) ->
      let t_params = List.map (fun _ -> fresh_var level) params in
      let fun_env = List.fold_left2
        (fun env param t_param -> Env.add param t_param env)
        env params t_params
      in
      let t_ret = w fun_env level body in
      TFun(t_params, t_ret)
  (* app *)
  | App(fn, args) ->
      let t_fn = w env level fn in
      let t_args = List.map (w env level) args in
      let arity = List.length args in
      let t_params, t_return = match_fun_type arity t_fn in
      List.iter2 unify t_params t_args;
      t_return
  (* let *)
  | Let(name, value, body) ->
      (* create a deeper-level scope *)
      let t_value = w env (level + 1) value in
      let t_value_poly = generalize level t_value in
      w (Env.add name t_value_poly env) level body

and match_fun_type arity = function
  | TFun(t_params, t_return) ->
      if (List.length t_params <> arity)
      then raise @@ Runtime_error("function arity mismatch")
      else (t_params, t_return)
  | TVar {contents = Bound t} -> match_fun_type arity t
  | TVar ({contents = Unbound(id, level)} as v) ->
      let rec loop = function
        | 0 -> []
        | n -> (fresh_var level) :: loop (n - 1)
      in
      let t_params = loop arity in
      let t_return = fresh_var level in
      v := Bound (TFun(t_params, t_return));
      (t_params, t_return)
  | _ -> raise @@ Runtime_error("application with non-function")

(* type parser *)

let replace_consts vars t =
  let env = List.fold_left
    (fun env v -> Env.add v (fresh_poly_var ()) env)
    Env.empty vars
  in
  let rec walk = function
    | TConst k when Env.mem k env -> Env.find k env
    | TApp(fn, args) -> TApp(walk fn, List.map walk args)
    | TFun(params, body) -> TFun(List.map walk params, walk body)
    | t -> t
  in
  walk t

let ty_ident = (spaces >> initial <~> many subseqt) => implode >>= function
  | "forall" -> mzero
  | s -> return s

let rec ty input =
  (fun_or_simple <|> paren_fun_ty) input
and fun_or_simple input =
  let maybe_fun t =
    (token "->" >> ty >>= fun body -> return (TFun([t], body))) <|> return t
  in
  (simple_ty >>= maybe_fun) input
and simple_ty input =
  let rec foldl t =
    (bracks (comma_list1 ty) >>= fun a -> foldl (TApp(t, a))) <|> return t
  in
  (prim_ty >>= foldl) input
and prim_ty input =
  ((ty_ident => fun k -> TConst k) <|> (token "(" >> ty << token ")")) input
and paren_fun_ty input =
  (parens (comma_list ty) >>= fun params ->
   token "->"             >>
   ty                     >>= fun body ->
   return (TFun(params, body))) input

let ty_forall =
  token "forall" >>
  token "["      >>
  many ty_ident  >>= fun vars ->
  token "]"      >>
  ty             >>= fun body ->
  return (replace_consts vars body)

let parse_ty = parse (ty_forall <|> ty)

(* infer *)

let make_env () =
  let assume name ty_str env =
    match parse_ty (LazyStream.of_string ty_str) with
    | None -> raise Syntax_error
    | Some t -> Env.add name t env
  in
    Env.empty
    |> assume "head" "forall[a] list[a] -> a"
    |> assume "tail" "forall[a] list[a] -> list[a]"
    |> assume "nil" "forall[a] list[a]"
    |> assume "cons" "forall[a] (a, list[a]) -> list[a]"
    |> assume "cons_curry" "forall[a] a -> list[a] -> list[a]"
    |> assume "map" "forall[a b] (a -> b, list[a]) -> list[b]"
    |> assume "map_curry" "forall[a b] (a -> b) -> list[a] -> list[b]"
    |> assume "one" "int"
    |> assume "zero" "int"
    |> assume "succ" "int -> int"
    |> assume "plus" "(int, int) -> int"
    |> assume "eq" "forall[a] (a, a) -> bool"
    |> assume "eq_curry" "forall[a] a -> a -> bool"
    |> assume "not" "bool -> bool"
    |> assume "true" "bool"
    |> assume "false" "bool"
    |> assume "pair" "forall[a b] (a, b) -> pair[a, b]"
    |> assume "pair_curry" "forall[a b] a -> b -> pair[a, b]"
    |> assume "first" "forall[a b] pair[a, b] -> a"
    |> assume "second" "forall[a b] pair[a, b] -> b"
    |> assume "id" "forall[a] a -> a"
    |> assume "const" "forall[a b] a -> b -> a"
    |> assume "apply" "forall[a b] (a -> b, a) -> b"
    |> assume "apply_curry" "forall[a b] (a -> b) -> a -> b"
    |> assume "choose" "forall[a] (a, a) -> a"
    |> assume "choose_curry" "forall[a] a -> a -> a"

(* main *)
let () =
  match parse_expr (LazyStream.of_channel stdin) with
  | None -> raise Syntax_error
  | Some exp ->
      let env = make_env () in
      reset_id ();
      exp |> w env 0 |> generalize (-1) |> string_of_ty |> print_endline
