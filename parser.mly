%{
   open Syntax
 %}
 
  /* Lexemes */
	%token <string> VAR
  %token And
  %token Or
  %token Neg
  %token Forall
  %token Exists
  %token LPAREN
  %token RPAREN
	%token LSQ
	%token RSQ
  %token EOF
  
  /* Precedence and associativity */
  %left And Or
  %nonassoc Neg
  %nonassoc Forall Exists
  /* Top level rule */
  %start toplevel
  %type <Syntax.expression> toplevel
  %%
  
  /* Grammar */
  
  toplevel: expression EOF { $1 }
  ;
  
	expression:
	|	VAR															 { Var($1) }
  | Neg expression                      { Neg($2) }
  | expression And expression         { And($1, $3) }
  | expression Or expression  { Or($1, $3) }
  | LPAREN expression RPAREN  { $2 }
	| Forall LSQ expression RSQ expression	{ $5 }
	| Exists LSQ expression RSQ expression	{ $5 }
	;		
	