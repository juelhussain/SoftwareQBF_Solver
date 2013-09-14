type token =
  | EOF
  | WORDS of (string)

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string list
