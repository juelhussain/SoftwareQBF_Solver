
module Convert = 
	struct
	open Syntax
  
  
  (*type expression =
        | False
        | True
        | Var of int
        | And of expression * expression (* && *)
        | Or of expression * expression (* || *)
        | Imp of expression * expression (* ->, implication *)
        | BImp of expression * expression (* <->, bi-implication *)
        | Neg of expression (* -, negation *)
  			| Forall of string * expression
        | Exists of string * expression
  
  
  let rec formula_to_expression fm =
    match fm with
      False -> False
    | True -> True
  	| Var(p) -> Var(int_of_string p)
    | Neg(p) -> Neg(convert_var_int p)
    | And(p,q) -> And(convert_var_int p,convert_var_int q)
    | Or(p,q) -> Or(convert_var_int p, convert_var_int q)
    | Imp(p,q) -> Imp(convert_var_int p, convert_var_int q)
    | BImp(p,q) -> BImp(convert_var_int p, convert_var_int q)
  	| Forall(p,q) -> Forall(convert_var_int p, convert_var_int q)
  	| Exists(p,q) -> Exists(convert_var_int p, convert_var_int q)*)
  
  
  
  	
  	(* Thers natural ordering on strings that can be utilised for *)
  	(* Variable renaming. To introduce order. So "b">"a"=true similarly *)
  	(* even though "b">"b" is false "bb"> "b" = true - Awesome!*)
  	
  	(* let global_vars_order_ht_t = Hashtbl.create 15;; *)
  	(* let global_vars_order_ht_h = Hashtbl.create 15;; *)
  	
  		
  (* in insert_to_ht list in Hashtbl.find global_vars_order_ht_h p;; *)
  	
  	(* Sort the variable occurances by OCaml natural ordering *)
  	let rec sort lst =
     match lst with
       [] -> []
     | head :: tail -> insert head (sort tail)
   and insert elt lst =
     match lst with
       [] -> [elt]
     | head :: tail -> if elt <= head then elt :: lst else head :: insert elt tail
  	
  	
  	(* Add the given variable to list and sort will sort the list let        *)
  	(* checkVar p list1 = let list1 = p::list1 in sort list1;;               *)
  		
  	(* let checkVarWInt p list1 = try ( let list1 = (int_of_string p)::list1   *)
  	(* in list1 ) with Failure str -> let list1 = p::list1 in list1            *)
  		
  		
  	(* Add variable to list and return list *)	
  	let checkVar p list1 =
  		let list1 = p:: list1 in list1
  	
		
		(* Add varibales to list and then sort at the end *)
  	let rec get_vars v list = 
  		let sortedList = 
  		match v with
  		| False -> let list = "-1":: list in list
  		| True -> let list = "-2":: list in list
  		| Var(p) -> let list = (checkVar p list) in list
  		| Neg(p) -> let list = (get_vars p list) in list
      | And(p, q) -> let list = (get_vars p list) in 
  		let list = (get_vars q list) in list
      | Or(p, q) -> let list = (get_vars p list) in 
  		let list = (get_vars q list) in list
      | Imp(p, q) -> let list = (get_vars p list) in 
  		let list = (get_vars q list) in list
      | BImp(p, q) -> let list = (get_vars p list) in let list = (get_vars q list) in list
    	| Forall(p, q) -> let list = (checkVar p list) in let list = (get_vars q list) in list
    	| Exists(p, q) -> let list = (checkVar p list) in let list = (get_vars q list) in list
  		in sort sortedList
  	
  	(* Insert the variable list in to the Hashtable and then replace*)
		(* the variable with hashtable index (List **not used at the moment)  *)
  	let insert_to_ht list (global_vars_order_ht_h) (global_vars_order_ht_t)= 
  		Hashtbl.clear global_vars_order_ht_t;
  		Hashtbl.clear global_vars_order_ht_h;
  		let rec insert i j list2 = 
  			if i > ((List.length list)) then (List.rev list2)
  			else
  				begin
  					if (Hashtbl.mem (global_vars_order_ht_h) (List.nth list (i-1)))
						then insert (i+1) (j) (list2) 
						else
							begin
    						Hashtbl.add global_vars_order_ht_t j (List.nth list (i -1));
      					Hashtbl.add global_vars_order_ht_h (List.nth list (i -1)) j;
      					insert (i +1) (j+1) (i:: list2) 
  						end
  				end
  				in insert 1 1 []
  	
		(* Replace the variables by the Hashtable index *)
  	let rec replace_vars v h = 
  		match v with
  		| False -> False
  		| True -> True
  		| Var(p) -> Var(string_of_int (Hashtbl.find h p))
  		| Neg(p) -> Neg(replace_vars p h)
      | And(p, q) -> And(replace_vars p h, replace_vars q h)
      | Or(p, q) -> Or(replace_vars p h, replace_vars q h)
      | Imp(p, q) -> Imp(replace_vars p h, replace_vars q h)
      | BImp(p, q) -> BImp(replace_vars p h, replace_vars q h)
    	| Forall(p, q) -> Forall(string_of_int (Hashtbl.find h p), replace_vars q h)
    	| Exists(p, q) -> Exists(string_of_int (Hashtbl.find h p), replace_vars q h)
  
  
  	let convertFormula fm (h) (t)= 
  		let intList = insert_to_ht(get_vars fm []) (h) (t) in
  		replace_vars (fm) (h)
  		
	end ;;
	
	
	
	
	
	
	