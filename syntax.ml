(*module Syntax =
struct*)
	
	type bdd = Zero | One | Node of int * bdd * bdd;;
	
	type expression = 
		| False 
		| True 
		| Var of string 
		| And of expression * expression 
		| Or of expression * expression
		| Imp of expression * expression 
		| BImp of expression * expression 
		| Neg of expression 
		| Forall of string * expression 
		| Exists of string * expression

	
	let rec print_exp (exp) = match exp with 
    | True -> print_string "True"
    | False -> print_string "False"
    | Var x -> Printf.printf "Var %s" x
    | And(x,y) ->  print_string "And ("; print_exp x ;print_string " , "; print_exp y ;print_string ")"
    | Or(x,y) ->  print_string "Or ("; print_exp x ;print_string " , "; print_exp y ;print_string ")"
    | Imp(x,y) ->  print_string "Imp ("; print_exp x ;print_string " , "; print_exp y ;print_string ")"
    | BImp(x,y) ->  print_string "BImp ("; print_exp x ;print_string " , "; print_exp y ;print_string ")"
    | Neg(x) -> print_string "Neg ("; print_exp x; print_string ")"
		| Forall(x,y) -> print_string ("Forall["^x^"] ("); print_exp y; print_string " )" 
		| Exists(x,y) -> print_string ("Exists["^x^"] ("); print_exp y; print_string " )" 

	let rec print_bdd (a) = match a with 
		| Zero ->  " Zero "
		| One ->  " One "
		| Node(x,y,z) ->  " Node ( " ^ (string_of_int x) ^ "," ^ print_bdd y ^ "," ^ print_bdd z ^ " ) "
	
	let time1 string_label f x =
    let t = Sys.time() in
    let fx = f x in
    Printf.printf "Execution time %s: %fs\n" (string_label) (Sys.time() -. t);
    fx;;

	let time2 string_label f x y =
    let t = Sys.time() in
    let fxy = f x y in
    Printf.printf "Execution time %s: %fs\n" (string_label) (Sys.time() -. t);
    fxy;;

	let time3 string_label f x y z =
    let t = Sys.time() in
    let fxyz = f x y z in
    Printf.printf "\n-------------\nExecution time %s: %fs\n-------------\n" (string_label) (Sys.time() -. t);
    fxyz
		
	let time4 string_label f w x y z =
    let t = Sys.time() in
    let fwxyz = f w x y z in
    Printf.printf "\n-------------\nExecution time %s: %fs\n-------------\n" (string_label) (Sys.time() -. t);
    fwxyz
		
	let time5 string_label f v w x y z =
    let t = Sys.time() in
    let fvwxyz = f v w x y z in
    Printf.printf "\n-------------\nExecution time %s: %fs\n-------------\n" (string_label) (Sys.time() -. t);
    fvwxyz
	
(*end;;*)