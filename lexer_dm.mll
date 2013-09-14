{
	open Parser_dm
}

rule tokens = parse
	| '\n' {tokens lexbuf}
	| '0' {tokens lexbuf}
	| ' '+'0' {ZERO(Lexing.lexeme lexbuf)}
	| 'p' [^ '\n']+ as header {HEADER(header)}
	| 'e' [^ '\n']+ as exists {EXISTS(exists)}
	| 'a' [^ '\n']+ as forall {FORALL(forall)}
	| eof {EOF}
	| [^'p' '\n' 'e' 'a']+ as words {WORDS(words)}



	
	