
module Translate =
struct
	
	open Syntax;;
	exception TerminalNode;;
	
	(* get variable number of a node *)
	let var (u : int) t =
		match Hashtbl.find t u with
		| Node(i, _, _) -> i
		| Zero | One -> raise TerminalNode
	;;
	
	(* get the left branch of a node *)
	let low (u : int) t =
		match Hashtbl.find t u with
		| Zero -> Zero
		| One -> One
		| Node(_, l, _) -> l
	;;
	
	(* get the high branch of a node *)
	let high (u : int) t : bdd =
		match Hashtbl.find t u with
		| Zero -> Zero
		| One -> One
		| Node(_, _, h) -> h
	
	(* check if node is decider *)
	let isDecider (bdd_input) =
		let rec decide input i =
			if (i >2) then
				false
			else
				match input with
				| One -> true
				| Zero -> true
				| Node(x, y, z) -> if (decide (y) (i +1)) then
							true
						else if (decide(z) (i +1)) then
							true
						else
							false
		in decide (bdd_input) (1)
	
	(* check if high(node) is one *)
	let deciderVal (bdd_input) =
		let rec decide1 input i =
			if (i >2) then
				false
			else
				match input with
				| One -> true
				| Zero -> false
				| Node(x, _, z) -> if (decide1 (z) (i +1)) then
							true
						else
							false
		in decide1 (bdd_input) (1)
	
	(* Check how many levels before reaching one through high nodes *)
	let deciderNo (bdd_input) =
		let rec decide1 input i =
			if (i >10) then
				i +1
			else
				match input with
				| One -> i
				| Zero -> i
				| Node(x, _, z) -> let new_i = (decide1 (z) (i +1) ) in
						if (new_i > i) then
							new_i
						else
							i
		in decide1 (bdd_input) (1)
	
	let checkChild (input) = match input with
		| One -> 1
		| Zero -> 0
		| Node(x, y, z) -> -1
	
	(* Check if it has atleast one terminal node. If it does then return   *)
	(* variable plus conjunction if low(var)=0 then return "var and" If    *)
	(* high(var)=0 then return "~var and" If low(var)=1 then return "~var  *)
	(* or" If high(var)=1 then return "var or"                             *)
	let rule_two (inputBDD) =
		let rec decide2 input i =
			match input with
			| Node(x, y, z) -> if (checkChild(y) = 0) then
						(string_of_int x^" and ")
					else if (checkChild(z) = 0) then
						("~"^string_of_int x ^ " and " )
					else if (checkChild(y) = 1) then
						("~"^string_of_int x^" or ")
					else if (checkChild(z) = 1) then
						(string_of_int x^" or ")
					else "error"
			| _ -> raise (Failure "input is not an expected node")
		in decide2 (inputBDD) (1)
	
	let translate_to_cnf (inputBDD) =
		let rec translate bdd1 =
			match bdd1 with
			| One -> ""
			| Zero -> ""
			| Node(x, y, z) ->
					(if isDecider(Node(x, y, z)) then
							(
								(if (deciderVal(Node(x, y, z))) then
										if (deciderNo(y) > 1) then
											(string_of_int x)^" or "
										else (string_of_int x)
									else if (deciderNo(Node(x, y, z)) <3) then ("~"^string_of_int x)
									else rule_two (Node(x, y, z)))^(translate y)^(translate z)
							)
						else if (isDecider(y)) then "rule 3"
						(* Check if the child of this node is decider if so then     *)
						(* apply rule 3                                              *)
						else "rule 4"
						(* If the child is not a decider then need to find which child is and      *)
						(* until a terminal node is reached then recursively call function and     *)
						(* find the path to that node and use them to construct the CNF as per     *)
						(* rule 3.                                                                 *)
					)
		in translate (inputBDD)
end;;