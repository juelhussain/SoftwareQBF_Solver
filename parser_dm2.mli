type token =
  | EOF
  | WORDS of (string)
  | HEADER of (string)
  | EXISTS of (string)
  | FORALL of (string)
  | ZERO of (string)

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string list
