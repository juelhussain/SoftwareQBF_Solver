
	open Syntax	
	
	
	let rec print_exp_asis (exp) = match exp with 
    | True -> print_string "True"
    | False -> print_string "False"
    | Var x -> Printf.printf "Var %s" x
    | And(x,y) ->  print_string "And ("; print_exp_asis x ;print_string " , "; print_exp_asis y ;print_string ")"
    | Or(x,y) ->  print_string "Or ("; print_exp_asis x ;print_string " , "; print_exp_asis y ;print_string ")"
    | Imp(x,y) ->  print_string "Imp ("; print_exp_asis x ;print_string " , "; print_exp_asis y ;print_string ")"
    | BImp(x,y) ->  print_string "BImp ("; print_exp_asis x ;print_string " , "; print_exp_asis y ;print_string ")"
    | Neg(x) -> print_string "Neg ("; print_exp_asis x; print_string ")"
		| Exists(_,_) -> print_string "exists"
		| Forall(_,_) -> print_string "forall"

	let rec print_exp (exp) = match exp with 
    | True -> print_string "True"
    | False -> print_string "False"
    | Var x -> Printf.printf " %s" x
    | And(x,y) ->  print_exp x ;print_string " /\\  "; print_exp y 
    | Or(x,y) -> print_exp x ;print_string " \\/ "; print_exp y 
    | Imp(x,y) ->  print_exp x ;print_string " -> "; print_exp y 
    | BImp(x,y) ->   print_exp x ;print_string " <-> "; print_exp y 
    | Neg(x) -> print_string " ~ "; print_exp x
		| Exists(_,_) -> print_string "exists"
		| Forall(_,_) -> print_string "forall"

	
	let rec print_bdd (a: bdd) = match a with 
		| Zero -> print_string " Zero "
		| One ->  print_string " One "
		| Node(x,y,z) ->  print_string (" Node ( " ^ (string_of_int x) ^ ","); print_bdd y;
		print_string ","; print_bdd z; print_string " ) "
	
	let rec get_print_exp_asis (exp) = match exp with 
    | True ->  "True"
    | False ->  "False"
    | Var x ->  "Var "^ x
    | And(x,y) ->   "And ("^ get_print_exp_asis x ^ " , "^ get_print_exp_asis y ^ ")"
    | Or(x,y) ->   "Or ("^ get_print_exp_asis x ^ " , "^ get_print_exp_asis y ^ ")"
    | Imp(x,y) ->   "Imp ("^ get_print_exp_asis x ^ " , "^ get_print_exp_asis y ^ ")"
    | BImp(x,y) ->   "BImp ("^ get_print_exp_asis x ^ " , "^ get_print_exp_asis y ^ ")"
    | Neg(x) ->  "Neg ("^ get_print_exp_asis x^  ")"
		| Exists(_,_) ->  "exists"
		| Forall(_,_) ->  "forall"

	let rec get_print_exp (exp) = match exp with 
    | True ->  "True"
    | False ->  "False"
    | Var x ->  x
    | And(x,y) ->  get_print_exp x ^ " /\\  "^ get_print_exp y 
    | Or(x,y) -> get_print_exp x ^ " \\/ "^ get_print_exp y 
    | Imp(x,y) ->  get_print_exp x ^ " -> "^ get_print_exp y 
    | BImp(x,y) ->   get_print_exp x ^ " <-> "^ get_print_exp y 
    | Neg(x) ->  " ~ "^ get_print_exp x
		| Exists(_,_) ->  "exists"
		| Forall(_,_) ->  "forall"
	
	let rec get_exp_qdimacs (exp) = match exp with 
    | Var x ->  x
    | And(x,y) ->  get_exp_qdimacs x ^ " 0\n"^ get_exp_qdimacs y 
    | Or(x,y) -> get_exp_qdimacs x ^ " "^ get_exp_qdimacs y 
    | Neg(x) ->  "-"^ get_exp_qdimacs x
		| Exists(x,_) ->  if ((String.sub x ((String.length x) - 1) 1) = "0") then 
			"e "^(String.sub x (0) ((String.length x)-2))
			else "e "^x
		| Forall(y,_) ->  if ((String.sub y ((String.length y) - 1) 1) = "0") then 
			"a "^(String.sub y (0) ((String.length y)-2))
			else "a "^y
		| True -> "T"
		| False -> "F"
		| _ -> raise (Failure "(Print) Given formula not in correct format\n")
	
	let rec get_print_bdd (a: bdd) = match a with 
		| Zero ->  " Zero "
		| One ->  " One "
		| Node(x,y,z) ->  " Node ( " ^ (string_of_int x) ^ "," ^ get_print_bdd y ^ "," ^ get_print_bdd z ^ " ) "
	
	
	let rec print_string_list = function 
	[] -> ()
	| e::l -> print_string e ; print_endline " " ; print_string_list l;;

	let rec print_exp_list = function 
	[] -> ()
	| e::l -> print_exp e ; print_endline " " ; print_exp_list l;;

	let rec print_exp_list_asis = function 
	[] -> ()
	| e::l -> print_exp_asis e ; print_endline " " ; print_exp_list_asis l;;
	
	let rec print_bdd_list = function 
	[] -> ()
	| e::l -> print_bdd e ; print_endline " " ; print_bdd_list l;;


	let rec get_print_exp_list = function 
	[] -> "\n"
	| e::l -> (get_print_exp e) ^ " \n" ^ (get_print_exp_list l);;

	let rec get_print_exp_list_asis = function 
	[] -> ""
	| e::l -> get_print_exp_asis e ^ " " ^ get_print_exp_list_asis l;;
	
	let rec get_print_bdd_list = function 
	[] -> "\n"
	| e::l -> get_print_bdd e ^ " \n" ^ get_print_bdd_list l;;

	let rec get_exp_list_to_qdimacs = function 
	[] -> ""
	| e::l -> get_exp_qdimacs e ^ " 0\n" ^ get_exp_list_to_qdimacs l;;


