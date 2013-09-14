{
	open Parser_dm	
}

rule tokens = parse
	| '\n' {tokens lexbuf}
	| '0' {tokens lexbuf}
	| eof {EOF}
	| [^'\n' '0']+ as words {WORDS(words)}
	
	{
		let run x = let lb =
			Lexing.from_channel x in
			Parser_dm.main tokens lb	
	}
	