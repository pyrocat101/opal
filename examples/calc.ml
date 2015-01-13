open Opal

let rec expr input = chainl1 term addop input
and term input = chainl1 factor mulop input
and factor input = (parens expr <|> integer) input
and mulop input = ((char '*' >> return ( * )) <|> (char '/' >> return (/))) input
and addop input = ((char '+' >> return (+)) <|> (char '-' >> return (-))) input
and parens expr = between (char '(') (char ')') expr
and integer input = (many1 digit >>= fun n -> return @@ int_of_string @@ implode n) input

let () =
  let input = LazyStream.of_channel stdin in
  match parse expr input with
  | Some ans -> Printf.printf "%d\n" ans
  | None -> print_endline "ERROR!"
