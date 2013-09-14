%{ 
 open Syntax 
%}

/* Lexemes */
%token Zero
%token <string> NegInt PosInt
%token  EOF
%start main
%type <Syntax.clause list> main
%%

/* Grammar */
  main: clause EOF { $1 }
  ;
  
	clause:
	 clause NegInt {$1 :: $2}
	|NegInt  { [Not(int_of_string $1)] }
	
	;
	
	
	/*clause:
	| NegInt  { [Not(int_of_string $1)]::[] }
	| PosInt { [Pos(int_of_string $1)]::[] }
	;*/
	
	