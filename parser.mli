type token =
  | VAR of (string)
  | And
  | Or
  | Neg
  | Forall
  | Exists
  | LPAREN
  | RPAREN
  | LSQ
  | RSQ
  | EOF

val toplevel :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.expression
