# 1 "lexer_dm2.mll"
 
	open Parser_dm2	

# 6 "lexer_dm2.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\001\000\248\255\002\000\003\000\004\000\006\000\007\000\
    \255\255\008\000\009\000\014\000\005\000\010\000\011\000";
  Lexing.lex_backtrk = 
   "\255\255\008\000\255\255\255\255\255\255\255\255\008\000\001\000\
    \255\255\008\000\002\000\003\000\004\000\005\000\006\000";
  Lexing.lex_default = 
   "\001\000\001\000\000\000\014\000\013\000\012\000\001\000\001\000\
    \000\000\001\000\001\000\001\000\012\000\013\000\014\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\008\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\000\000\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \006\000\000\000\000\000\000\000\000\000\000\000\009\000\000\000\
    \009\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \007\000\000\000\000\000\000\000\000\000\000\000\010\000\000\000\
    \011\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\255\255\000\000\000\000\004\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \005\000\255\255\000\000\255\255\000\000\000\000\255\255\255\255\
    \255\255\255\255\000\000\000\000\000\000\000\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \002\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\000\000\255\255";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\001\000\003\000\004\000\005\000\012\000\
    \006\000\007\000\009\000\010\000\013\000\014\000\255\255\255\255\
    \011\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\006\000\255\255\
    \009\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\006\000\255\255\
    \009\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\001\000\255\255\255\255\000\000\001\000\006\000\
    \007\000\009\000\010\000\006\000\007\000\009\000\010\000\011\000\
    \000\000\001\000\255\255\011\000\255\255\255\255\006\000\007\000\
    \009\000\010\000\255\255\255\255\255\255\255\255\011\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\001\000\003\000\004\000\005\000\012\000\006\000\007\000\
    \009\000\010\000\013\000\014\000\255\255\255\255\011\000";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec tokens lexbuf =
    __ocaml_lex_tokens_rec lexbuf 0
and __ocaml_lex_tokens_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 6 "lexer_dm2.mll"
        (tokens lexbuf)
# 108 "lexer_dm2.ml"

  | 1 ->
# 7 "lexer_dm2.mll"
       (tokens lexbuf)
# 113 "lexer_dm2.ml"

  | 2 ->
# 8 "lexer_dm2.mll"
        (tokens lexbuf)
# 118 "lexer_dm2.ml"

  | 3 ->
# 9 "lexer_dm2.mll"
           (ZERO(Lexing.lexeme lexbuf))
# 123 "lexer_dm2.ml"

  | 4 ->
let
# 10 "lexer_dm2.mll"
                    header
# 129 "lexer_dm2.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 10 "lexer_dm2.mll"
                           (HEADER(header))
# 133 "lexer_dm2.ml"

  | 5 ->
let
# 11 "lexer_dm2.mll"
                     exists
# 139 "lexer_dm2.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 11 "lexer_dm2.mll"
                            (EXISTS(exists))
# 143 "lexer_dm2.ml"

  | 6 ->
let
# 12 "lexer_dm2.mll"
                    forall
# 149 "lexer_dm2.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 12 "lexer_dm2.mll"
                           (FORALL(forall))
# 153 "lexer_dm2.ml"

  | 7 ->
# 13 "lexer_dm2.mll"
       (EOF)
# 158 "lexer_dm2.ml"

  | 8 ->
let
# 14 "lexer_dm2.mll"
                           words
# 164 "lexer_dm2.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 14 "lexer_dm2.mll"
                                 (WORDS(words))
# 168 "lexer_dm2.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_tokens_rec lexbuf __ocaml_lex_state

;;

# 16 "lexer_dm2.mll"
  
		let run x = let lb =
			Lexing.from_channel x in
			Parser_dm2.main tokens lb	
	
# 180 "lexer_dm2.ml"
