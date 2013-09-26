
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
			
		
			let convert_clause_to_Expression clause =
				 if (clause.[0]='e') then Exists((split_and_concat ('e') clause), True)
					else if (clause.[0]='a') then Forall((split_and_concat ('a') clause), True)
					else if (clause.[0]='p') then False
					else (string_to_disjunction clause)					
			
	
			(* returns the element at position k of list given *)
    let rec at k = function
        | [] -> raise (Failure "at: List is empty")
        | h :: t -> if k = 1 then h else at (k-1) t
		
		
		
		let zero_list j =
			let rec add_zero list1 i =
				if (i>=j) then list1 else
				let list1 = "0"::list1 in  
				add_zero list1 (i+1)
				in add_zero [] 0
			
		let switch_rows (ht_stats) (ht) (index) (clause_max_index) (length_clause) =
			let chosen_val = Hashtbl.find ht clause_max_index in
			let redundant_val = Hashtbl.find ht index in
			let redundant_val_stats = Hashtbl.find ht_stats index in
			Hashtbl.add ht clause_max_index redundant_val;
			Hashtbl.add ht (index) (chosen_val);
			Hashtbl.add ht_stats index (zero_list length_clause);
			Hashtbl.add ht_stats clause_max_index (redundant_val_stats)
			;; 
		
		(* This will return the clause index of clause list that has the highest*)
		(* occurence of variable given as input*)
		(* stats = ["1";"2";"7"]["3";"7";"1"]*)
		(* input 3 ht 2 *)
		(* Then output would be 2 as the second list has highest var 2 count.*)
		(* This cooresponds to the clause set in ht_clauses. *)
		let get_var_ht length ht var =
			(*Order the clauses according to given variable number*)
			let index_no =
			let rec find_clause num i index =
				if (i> length) then index 
				else 
					begin
    				let clause = Hashtbl.find ht i in
    				let max = (at var clause) in
    				if ((int_of_string max) > num) then find_clause (int_of_string max) (i+1) (i) else find_clause num (i+1) (index)
  				end
				in find_clause (-1) 1 (1)
			(*in let index_no = switch_rows ht index_no 1*) in index_no
			
			
		let get_max_var (clauseList) =
			let rec iter clause i var_i= 
				if (i = (List.length clauseList)) then 
					 "complete\n"
    			else
						begin
							let varList = (split ' ' clause) in
							for j = 1 to (List.length varList) do
								Printf.printf "not complete %s\n" (at j varList)
								done;
								("test"^(at i clauseList));
							iter (at (i+1) (clauseList)) (i+1) var_i							
						end
				 in iter (at 1 clauseList) (1) (-1)	
		
		let remove_neg var =
			if (String.sub var 0 1="-") then 
					let var = (String.sub var 1 ((String.length var)-1)) 	in var
			else var 
		
		
		
		let sum_vars_ht (ht) (length_ht) (length_ht_clauses) =
			let rec sum_vars new_list j =
				if (j>(length_ht_clauses)) then (List.rev new_list)
				else
				begin
  				let var_count =
						let rec sum_clause var i =
							(if (i>length_ht) then var
      				else
      					begin
									let clause = Hashtbl.find ht i in		
          						let var = var + (int_of_string (at j clause)) in
          						sum_clause (var) (i+1)
      						end)
      				in sum_clause 0 1;
							in sum_vars (var_count::new_list) (j+1)
					end
		in sum_vars [] 1
		
		let max_var_int_list int_list =
			let rec max var i =
				if (i>=(List.length int_list)) then var 
				else
					begin
						let var1 = (List.nth int_list i) in
						if (var1 > var) then max (var1) (i+1) else max (var) (i+1)
					end				
				in max (if(List.length int_list>0) then List.nth int_list 0 else -1) 1
		
		let max_var_int_index int_list =
			let rec max var i index =
				if (i>(List.length int_list)) then index
				else
					begin
						let var1 = (at i int_list) in
						if (var1 > var) then max (var1) (i+1) (i) else max (var) (i+1) (index)
					end				
				in max (at 1 int_list) (1) (1)
		
		(* As select_max_var but higher value takes priority*)			
		let get_max_var (clauseList) (max_or_total: int)=
			let clauseList = (List.rev ("0"::(List.rev clauseList))) in
    	let rec iter clause i var_i k= 
    		if (i >= (List.length clauseList)) then 
						if (max_or_total = 1) then  var_i else k
    			else
						begin
  						(*Split the clause up*)
  						let varList = (split ' ' clause) in
							let rec cycleVarList j =
								if (j=(List.length varList)) then 
									begin
										let var1 = (at j varList) in	
										let var = remove_neg var1 in 					
										if ((int_of_string var) > var_i) then 
											iter (at (i+1) (clauseList)) (i+1) (int_of_string (var)) (j+k)
										else
										 	iter (at (i+1) (clauseList)) (i+1) var_i (j+k)
									end
								else
										cycleVarList (j+1)
								in cycleVarList 1;
  					end 
    in iter (at 1 clauseList) (1) (-1) 0
		
		let get_vars_count var clause =
			let vars_clause = split ' ' clause in
			let rec cycle_clause_vars var_freq k = 
				if (k>(List.length vars_clause)) 
					then var_freq
				else 
  				begin
    				let var1 = (at k vars_clause) in	
    				let var1 = remove_neg var1 in 	
    				if((var)=(int_of_string var1)) 
      				then	cycle_clause_vars (var_freq+1) (k+1)
    				else 
    					cycle_clause_vars var_freq (k+1)
					end
			in cycle_clause_vars 0 1
		
		(* Take the clause list and the Hashtable. *)
		(* The ht has the index as the clause number while the list in the *)
		(* ht has the the variable number of 1 to list length.*)
		(* E.g. ht.find ht 1 will give list [1;2;,...,;n] which has 1 with an int*)
		(* value of the number of variables in the clause 1. While ht.find ht 2*)
		(* will give the number of the second clause. *)
		
		let order ht_stats ht_clauses clauseList = 
			(* Reorder the clause list according to sorting of clauses with max number of *)
			(* variable count.*)
			(* Hashtable length is set up first because manipulation of HT increases HT length*)
			(* without adding any values. This is because of immutable fields. When values are*)
			(* replaced they are merely added on top increasing size and removing the item recovers*)
			(* the previous item.*)
			let ht_length_clause = List.length (Hashtbl.find ht_stats 1) in
			let ht_length = Hashtbl.length ht_stats in
			(* 1- Add up all the variable count. *)
			(* 2- sort varibles according to max count. *)
  		(* 3- Order clause according to variables max occurence *)
  		let rec cycle_clause_set i =
  			if (i>ht_length) then (Hashtbl.find ht_clauses 1)
				else
					begin
    				let int_list_sum =	sum_vars_ht (ht_stats) (ht_length) (ht_length_clause) in
      			let variable_max_index = (max_var_int_index int_list_sum) in
      			let clause_max = get_var_ht ht_length ht_stats variable_max_index in
      			switch_rows (ht_stats ) (ht_clauses) (i) (clause_max) ht_length_clause;
						let clause_max2 = get_var_ht ht_length ht_stats  variable_max_index in
      			switch_rows (ht_stats ) (ht_clauses) (i+1) (clause_max2) ht_length_clause;
						cycle_clause_set (i+2)
  				end
			in cycle_clause_set 1;;
		
		(*Take the expressionList and reorder according to same variables.*)
		
		(* Latest design. This is a new feature where the clause list generated after*)
		(* build is now grouped to make the conjunction OBDD smaller. This works by *)
		(* groupng the clauses that have the same variables staring with the clause with*)
		(* the max number of a particular variable. The conjunction module then is run *)
		(* with clause pairs that have the maximal number of same variables. This will ensure *)
		(* the conjunction OBDD is shrunk at every call. Then the overall output of each pair*)
		(* has an implicit conjunction as before. *May run conjunction between the output *)
		(* conjunction OBDDs too*  *)
		
		
		(*Varibale grouping here to genrate a new expression list with *)
			(* clauses in order of same variables in frequency number. For example*)
			(* if original clauses was: 1 2 3; 4 5 6; 3 2 1; 6 5 4; rather than running conjunction *)
			(* with 1 and 2 which has different variables (so large OBDD) we run it with 1 and 3 *)
			(* and 2 and 4. Resulting in much smaller OBDDs especially if they were negation *)
			(* varables*)
			
		
		let order_clauses clauseList (ht_stats) (ht_clauses)= 
			(*let ht_stats= Hashtbl.create 15 in
			let ht_clauses = Hashtbl.create 15 in*)
			(*get max variable*)
			let max_var = get_max_var clauseList 1 in
			let rec cycle_clauses i = 
				if (i>(List.length clauseList)) then 
					(*Return the modified List*)
					begin
					print_string "leaving cycle_clauses\n";
					order ht_stats ht_clauses clauseList
					end
				else
					begin
    				(*count the occurences of the vars in the clause i*)
    				let clause = at i clauseList in
						let stats_list = 
      				let rec cycle_to_max_var var listFreq=
      						if (var> max_var) then (List.rev listFreq)
      						else
        						begin
      								(* Does the var exist in the clause? if yes then *)
          						(* add 1 to the position of list i *)
          						let var_freq = get_vars_count var clause
          						in 
											cycle_to_max_var (var+1) ((string_of_int (var_freq))::listFreq)
          						(*Hashtbl.add ht_statsi (var_freq::listFreq);*)
    								end
      					in cycle_to_max_var 1 []
							in 
							Hashtbl.add ht_stats i stats_list;
							Hashtbl.add (ht_clauses) (i) (clause);
  					  cycle_clauses (i+1)
					end
			in cycle_clauses 1
		
					
							
											
(*	end ;;*)
	
	
	
	
	
	
	