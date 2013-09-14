
%{
	
	let parse_error msg = Printf.eprintf "%s\n" msg
	
	%}
	
	%token EOF
	%token <string> WORDS
	%token <string> HEADER
	%token <string> EXISTS
	%token <string> FORALL
	%token <string> ZERO
 	%type <string list> main
	%start main
	
	%%
	
	main: 
	words EOF{$1}
	;
		
	words:
	| words ZERO {$1}
	| ZERO {[]}
	| HEADER {[$1]}
	| EXISTS {[$1]}
	| FORALL {[$1]}
	| WORDS {[$1]}
	| words WORDS {$2::$1}
	|	words HEADER {$2::$1}
	| words EXISTS {$2::$1}
	| words FORALL {$2::$1}
	;
	