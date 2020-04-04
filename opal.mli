module LazyStream :
sig
  type 'a t = Cons of 'a * 'a t Lazy.t | Nil
  val of_stream : 'a Stream.t -> 'a t
  val of_function : (unit -> 'a option) -> 'a t
  val of_string : string -> char t
  val of_channel : in_channel -> char t
end

val implode : char list -> string
val explode : string -> char list
val ( % ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

type ('token, 'result) parser
val parse : ('token, 'a) parser -> 'token LazyStream.t -> 'a option

val return : 'a -> ('token, 'a) parser
val ( >>= ) :
  ('token, 'a) parser -> ('a -> ('token, 'b) parser) -> ('token, 'b) parser
val ( let* ) :
  ('token, 'a) parser -> ('a -> ('token, 'b) parser) -> ('token, 'b) parser
val ( <|> ) : ('token, 'a) parser -> ('token, 'a) parser -> ('token, 'a) parser
val scan : ('token, 'a) parser -> 'token LazyStream.t -> 'a LazyStream.t
val mzero : ('token, 'a) parser
val any : ('token, 'token) parser
val satisfy : ('token -> bool) -> ('token, 'token) parser
val eof : 'a -> ('token, 'a) parser

val ( => ) : ('token, 'a) parser -> ('a -> 'b) -> ('token, 'b) parser
val ( << ) : ('token, 'a) parser -> ('token, 'b) parser -> ('token, 'a) parser
val ( >> ) : ('token, 'a) parser -> ('token, 'b) parser -> ('token, 'b) parser
val ( <~> ) :
  ('token, 'a) parser -> ('token, 'a list) parser -> ('token, 'a list) parser

val choice : ('token, 'a) parser list -> ('token, 'a) parser
val count : int -> ('token, 'a) parser -> ('token, 'a list) parser
val between :
  ('token, 'a) parser -> ('token, 'b) parser ->
  ('token, 'c) parser -> ('token, 'c) parser
val option : 'a -> ('token, 'a) parser -> ('token, 'a) parser
val optional : ('token, 'a) parser -> ('token, unit) parser
val skip_many1 : ('token, 'a) parser -> ('token, unit) parser
val skip_many : ('token, 'a) parser -> ('token, unit) parser
val many1 : ('token, 'a) parser -> ('token, 'a list) parser
val many : ('token, 'a) parser -> ('token, 'a list) parser
val sep_by1 :
  ('token, 'a) parser -> ('token, 'b) parser -> ('token, 'a list) parser
val sep_by :
  ('token, 'a) parser -> ('token, 'b) parser -> ('token, 'a list) parser
val end_by1 :
  ('token, 'a) parser -> ('token, 'b) parser -> ('token, 'a list) parser
val end_by :
  ('token, 'a) parser -> ('token, 'b) parser -> ('token, 'a list) parser
val chainl1 :
  ('token, 'a) parser -> ('token, 'a -> 'a -> 'a) parser -> ('token, 'a) parser
val chainl :
  ('token, 'a) parser -> ('token, 'a -> 'a -> 'a) parser -> 'a -> ('token, 'a) parser
val chainr1 :
  ('token, 'a) parser -> ('token, 'a -> 'a -> 'a) parser -> ('token, 'a) parser
val chainr :
  ('token, 'a) parser -> ('token, 'a -> 'a -> 'a) parser -> 'a -> ('token, 'a) parser

val exactly : 'token -> ('token, 'token) parser
val one_of : 'token list -> ('token, 'token) parser
val none_of : 'token list -> ('token, 'token) parser
val range : 'token -> 'token -> ('token, 'token) parser

val space : (char, char) parser
val spaces : (char, unit) parser
val newline : (char, char) parser
val tab : (char, char) parser
val upper : (char, char) parser
val lower : (char, char) parser
val digit : (char, char) parser
val letter : (char, char) parser
val alpha_num : (char, char) parser
val hex_digit : (char, char) parser
val oct_digit : (char, char) parser

val lexeme : (char, 'a) parser -> (char, 'a) parser
val token : string -> (char, string) parser
