{open Parser_dimacs}

rule token = parse
  [' ' '\t' '\n'] {token lexbuf}

  | '-'['1'-'9']* {NegInt(Lexing.lexeme lexbuf)}
  | ['1'-'9']* {PosInt(Lexing.lexeme lexbuf)}
  |'0' {Zero}
(*comments *)
  | 'c' [^ '\n']* {token lexbuf}
  | 'p' [^ '\n']* {token lexbuf}
  | eof {EOF}


  rule tokens = parse 
    | ' ' [^ '\n']* {tokens lexbuf}
  	| 'e'			{ Exists }
    | ['0'-'9' 'A'-'Z' 'a'-'z' '_']* as s { VAR (s) }
  {
  	let _ = 
  		let lb = Lexing.from_channel (open_in Sys.argv.(1)) in
  		let rec getVars lexbuf vars =
  			let tok = tokens lexbuf in
  			match tok with 
  			| Exists -> Exists(getVars lexbuf vars)
  			| VAR(s) -> s
  			| eof -> print_endline "Construction of Existential formula complete."
  		in getVars lb '';;
  	}

