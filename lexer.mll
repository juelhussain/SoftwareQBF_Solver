{
   open Parser
	
  }
  
  rule lexeme = parse
     [' ' '\t' '\r' '\n']  { lexeme lexbuf }
   | '&'           { And }
  | '|'           { Or }
  | '!'           { Neg }
  | '('           { LPAREN }
  | ')'           { RPAREN }
	| '['						{ LSQ }
	| ']'						{ RSQ }
	| "forall"			{ Forall }
	| "exists"			{ Exists }
	| ['0'-'9' 'A'-'Z' 'a'-'z' '_']* as s { VAR (s) }
  | eof             { EOF }