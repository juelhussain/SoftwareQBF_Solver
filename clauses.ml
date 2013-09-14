


module Clauses = struct
	
	open Syntax;;
	
	(* type var = int;;

	type literal =
		| Pos of int
		| Neg of int;;
	
	type clause = literal list;;
	 *)
	
	let getClause1 (clause) = 
	List.nth clause 0;;
	
	let clause_to_string c =
		match c with
		| Pos(x) -> " +"^(string_of_int x)
		| Not(x) -> " -"^(string_of_int x)

	let translate_lit lit = 
		match lit with
		| Pos(x) -> Var (string_of_int x)
		| Not(x) -> Var (string_of_int x);;
	
end;;