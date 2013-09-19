
(*module Convert = 
	struct*)
	open Syntax;;
  
  
  	
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
  
  
	
	(* This will take a formula and replace the variable by a natural ordering that *)
	(* is tored in the hashtables and orginal formulas recovered from the *)
	(* Hashtbl index value. *)
  	let convertFormula fm (h) (t)= 
  		let intList = insert_to_ht(get_vars fm []) (h) (t) in
  		replace_vars (fm) (h)
  		
			
			
	let rec print_list = function 
	[] -> ()
	| e::l -> print_string e ; print_endline " " ; print_list l
	 
	let string_index str splitVal =
  try Some(String.index str splitVal)
  	with Not_found -> None
 
  let split splitter str =
    let rec cycle varslist str =
      match (string_index str splitter) with
      | Some i ->
          let this = 
						String.sub str 0 i						 
          and next = 
						if (1<(String.length str)-i) then 
						String.sub str (i+1) (String.length str - i - 1) 
						else "" 
							in
					if (this = "") then cycle (varslist) next
					else cycle (this::varslist) next
      | None ->
				if (str="") then List.rev varslist
				else begin
				 let finalList =  ( 
					if ((str.[0] = ' ')&&(String.length str>1))  
							then 
								begin
									(String.sub str 1 (String.length str -1))::varslist 
								end
						else str::varslist
						) 
						in
						List.rev finalList
					end
    in cycle [] str		
			
			
	
			
	let split_and_concat splitter1 str1 = 
		let itemsList = (split splitter1 str1) in
		let rec concatList new_str i = 
			if new_str = " " then concatList new_str (i+1)
			else if (i>(List.length itemsList)-1) then 
				new_str
			else
				begin
					let item = List.nth itemsList i in
						if (item = "0") then concatList (new_str) (i+1)
						else
						concatList (new_str^" "^item) (i+1)
				end
		in concatList (List.nth itemsList 0) 1
				
			
			let get_var var = 
				if ((int_of_string var) < 0 ) then 
  				begin
  					let newvar = (split_and_concat '-' var) in
  					Neg(Var newvar)
  				end
				else 
					(Var var) 
			
	let string_to_disjunction stringClause = 
		let listVars = split ' ' stringClause in 
		let rec processListVars lv i exp=
			if (i>(List.length lv)-1) then 
				exp
			else if (i>(List.length lv)-2) then 
				begin
					let finalvar = (List.nth lv i) in
					if ((int_of_string finalvar) = 0 ) then 
						begin
							processListVars lv (i+1) (exp) 
						end
					else if ((int_of_string finalvar) < 0 ) then 
						begin
							let newfinalvar = (split_and_concat '-' finalvar) in
							processListVars lv (i+1) (Or(exp, Neg(Var newfinalvar))) 
						end
						else processListVars lv (i+1) (Or(exp, Var finalvar)) 
				end
			else
				begin
					let var = (List.nth lv i) in
					if ((int_of_string var) < 0 ) then 
						begin
							let newvar = (split_and_concat '-' var) in
							processListVars lv (i+1) (Or(exp, Neg(Var newvar))) 
						end
						else if ((int_of_string var) = 0 ) then 
							processListVars lv (i+1) (exp) 
						else processListVars lv (i+1) (Or(exp, Var var)) 
				end
		in processListVars listVars 1 (get_var(List.nth listVars 0));;	
				
				
				
	(* This will take a string list that has clauses and convert each clause *)
	(* in to the expression representation and return an expression List*)
	let convert_clauses_to_ExpressionList clauseList =
		let rec getClause expressionList i =
			if (i>(List.length clauseList)-1) then List.rev expressionList
			else
				begin
				 let clause = (List.nth clauseList i) in 
					if (clause.[0]='e') then
						getClause (Exists((split_and_concat ('e') clause), True)::expressionList) (i+1)
					else if (clause.[0]='a') then
						getClause (Forall((split_and_concat ('a') clause), True)::expressionList) (i+1)
					else if (clause.[0]='p') then 
						getClause (expressionList) (i+1)
					else 
						begin
						 getClause ((string_to_disjunction clause)::expressionList) (i+1)
						end
				end
			in getClause [] 0;;			
			
			
			
			
				
	let convert_string_to_OR_clauseOld stringClause = 
		let listVars = split ' ' stringClause in 
		let rec processListVars lv i=
			if (i>(List.length lv)-1) then 
				print_endline "done" 
			else
				begin
					print_endline (List.nth lv i);
					processListVars lv (i+1)
				end
		in processListVars listVars 0;;	
			
	let covert_clauses_to_ExpressionListOld clauseList =
		let rec getClause expressionList i =
			if (i>(List.length clauseList)-1) then List.rev expressionList
			else
				begin
				 let clause = (List.nth clauseList i) in 
					if (clause.[0]='e') then
						getClause (True::expressionList) (i+1)
					else if (clause.[0]='p') then 
						getClause (False::expressionList) (i+1)
					else 
						begin
						 getClause (True::expressionList) (i+1)
						end
				end
			in getClause [] 0;;

  let string_to_disjunctionOld2 stringClause = 
  		let listVars = split ' ' stringClause in 
  		let rec processListVars lv i exp=
  			if (i>(List.length lv)-1) then 
  				exp
  			else if (i>(List.length lv)-2) then 
  				begin
  					let finalvar = (List.nth lv i) in
  					processListVars lv (i+1) (Or(exp, Var finalvar)) 
  				end
  			else
  				begin
  					let var = (List.nth lv i) in
  					let exp2 = Or(exp, Var var) in
  					processListVars lv (i+1) (exp2) 
  				end
  		in processListVars listVars 1 (Var (List.nth listVars 0));;	
					
(*	end ;;*)
	
	
	
	
	
	
	