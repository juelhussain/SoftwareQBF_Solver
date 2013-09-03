module Disjunction = 
	struct
		
		(* Add expression to given hashtable in given index i *)
		let add_to_ht (ht) (exp) (i: int) = Hashtbl.add ht i (exp)
		
		(* returns a list of all the right values in a pair stored in the*)
		(* given hashtable *)
		let right_ht (ht) = Hashtbl.fold (fun k v acc -> (
			match v with 
				| (_,x) -> x) :: acc) ht []
		let left_ht (ht) = Hashtbl.fold (fun k v acc -> (
			match v with 
				| (x,_) -> x) :: acc) ht []
			
		let add_to_list (a: bdd) (b: bdd list) = a::b
		
		(* removes the element n from list given retruning the modified list *)
    let drop list n =
    	let rec aux i = function
    		| [] -> []
    		| h::t -> if i = n then aux (n+1) t else h:: aux (i+1) t in
    	aux 1 list

		(* returns the element at position k of list given *)
    let rec at k = function
        | [] -> raise (Failure "List is empty")
        | h :: t -> if k = 1 then h else at (k-1) t
		
		(* returns a list of expressions by assigning low high evaluations, given*)
		(* variable i. For left/right option case false/true evaluations are given. *)
		let get_low_high_Vals (expList) (i) (hashtbl2) (leftOrRight: int)= 
    	(*for j=1 to List.length y do*)
    	let rec cycleNodes (j) =
    			if (j>(List.length expList)) then
    				begin
    					if leftOrRight = 1 then				
    						left_ht(hashtbl2)
    						else 
    						right_ht(hashtbl2)
    				end 				
    			else 
    			begin
    			(*is element labelled by max variable*)
    			let elementX = (at j expList) in
    			if (var_bool (Var i) elementX) then	
    				(
    					print_string ("Variable found.\n");
    					add_to_ht (hashtbl2) ((eval(var_lookup (Var i) elementX False)),(eval (var_lookup (Var i) elementX True))) (j);
    					cycleNodes (j+1) 
    				)
    			else
    				begin
    				print_string ("Variable in expression not found.\n");
    				add_to_ht (hashtbl2) (elementX,elementX) (j);
    				cycleNodes (j+1)
    				end
    			end
    in cycleNodes (1)
		
		(* Returns a list of Nodes with new bdd list eval either neg or pos: *)
		(* dependent on left or right *)
    let get_low_high_BDD (nodesList) (expList) (i) (hashtbl3) (h) (t) (leftOrRight: int) = 
    	(*for j=1 to List.length y do*)
    	let rec cycleNodes (j) =
    			if (j>(List.length nodesList)) then
    				begin
    					print_string "j more than list length\n";
    					if leftOrRight = 1 then				
    						left_ht(hashtbl3)
    					else 
    						right_ht(hashtbl3)
    				end 				
    			else 
    			begin
    			(*is element labelled by max variable*)
    			let elementX = (at j expList) in
    			if (var_bool (Var i) elementX) then	
    				(
    					print_string ("Variable found.\n");
    					add_to_ht (hashtbl3) ((build (eval(var_lookup (Var i) elementX False)) (h) (t) ),(build(eval (var_lookup (Var i) elementX True)) (h) (t))) (j);
    					cycleNodes (j+1) 
    				)
    			else
    				begin
    				print_string ("Variable in expression not found.\n");
    				add_to_ht (hashtbl3) ((at j nodesList),(at j nodesList)) (j);
    				cycleNodes (j+1)
    				end
    			end
    in cycleNodes (1)
					
					
		let get_low_high_list_Left (nList) (eList) (h) (t) (maxVariable: int) = 
    	get_low_high_BDD (nList) (eList) (maxVariable) (createHT 15) (h) (t) (1)
    
    let get_low_high_list_Right (nList) (eList) (h) (t) (maxVariable: int) = 
    	get_low_high_BDD (nList) (eList) (maxVariable) (createHT 15) (h) (t) (2) 
    
    let get_low_high_list_Vals_Left (eList) (maxVariable: int) = 
    	get_low_high_Vals (eList) (maxVariable) (createHT 15) (1)
    
    let get_low_high_list_Vals_Right (eList) (maxVariable: int) = 
    	get_low_high_Vals (eList) (maxVariable) (createHT 15) (2)		
			
		(* selects max variable in nodeslist. Lower value takes presedence.*)
		(* Also only first node is checked. Nested nodes are ignored - *)
		(* may have to revise *)
		let select_max_var (nodesList) =
    	let rec iter element i var_i= 
    		if (i = List.length nodesList) then begin Printf.printf "1 Value i: %d var_i: %d \n" i var_i ;
    		match element with	
    				| Node(x,_,_) -> if ((x < var_i) && not(x<0)) then x else var_i
    				| _ -> if var_i > -1 then var_i else -1
    		 end 
    			else
    				match element with	
    				| Node(x,_,_) -> 
    					if ((x < var_i) && not(x<0)) then 
    						begin
      						Printf.printf "2 Value i: %d var_i: %d \n" i var_i;
      						(iter (at (i+1) nodesList) (i+1) (x)) 
    						end
    					else 
    						begin 
      						Printf.printf "3 Value i: %d var_i: %d \n" i var_i;
      						(iter (at (i+1) nodesList) (i+1) (var_i)) 
    						end
    				| _ -> iter (at (i+1) nodesList) (i+1) var_i
    in iter (at 1 nodesList) (1) (99999)	
		
		(* As select_max_var but higher value takes priority*)			
		let select_max_var_high_priority (nodesList) =
    	let rec iter element i var_i= 
    		if (i = List.length nodesList) then begin Printf.printf "1 Value i: %d var_i: %d \n" i var_i ;
    		match element with	
    				| Node(x,_,_) -> if x > var_i then x else var_i
    				| _ -> if var_i > -1 then var_i else -1
    		 end 
    			else
    				match element with	
    				| Node(x,_,_) -> if x > var_i then begin
    					Printf.printf "2 Value i: %d var_i: %d \n" i var_i;
    					(iter (at (i+1) nodesList) (i+1) (x)) end
    					else begin 
    						Printf.printf "3 Value i: %d var_i: %d \n" i var_i;
    						(iter (at (i+1) nodesList) (i+1) var_i) end
    				| _ -> iter (at (i+1) nodesList) (i+1) var_i
    in iter (at 1 nodesList) (1) (-1)
		
		
		(* This gives a disjunction over bdds (nodes list) given. Takes global dag *)
		(* as input which holds each nodes from nodes list as bdd and returns *)
		(* the global dag in modified form. *)
		let disjunction (nList) (eList) (h) (t) =
    	let rec disjunction' (nodesList) (expList) (m1) = 
    		Printf.printf "Recursion m = %d\n" m1;
    		let rec cycleNodes m = 
    			Printf.printf "CycleNode m = %d\n" m;
    			let element = (at m nodesList) in 
    			if ((m1=1) || (m1<1)) then 
    				begin
    					print_string ("Not enough elements. Node only: \n"^(print_bdd element)^"\n"); element
    				end
    			else if (m>1) then
    			begin
    				match element with
    				| One -> print_string "One\n"; One
    				| Zero -> print_string "Zero\n"; disjunction' (drop nodesList m) (drop expList m) (m1-1)
    				| _ -> cycleNodes(m-1)
    			end
    			else
    				begin
    					match element with
    					| One -> print_string "One\n"; One
    					| Zero -> print_string "Zero\n"; disjunction' (drop nodesList m) (drop expList m) (m1-1)
    					| _ -> let maxVar = select_max_var (nodesList) in 
    						let left = get_low_high_list_Left (nodesList) (expList) (h) (t) (maxVar) in
    						let right = get_low_high_list_Right (nodesList) (expList) (h) (t) (maxVar) in
    						let leftX = get_low_high_list_Vals_Left (expList) (maxVar) in
    						let rightX = get_low_high_list_Vals_Right (expList) (maxVar) in
    						let k1 = disjunction' (left) (leftX) (List.length left) in
    						let k2 = disjunction' (right) (rightX) (List.length right) in
    						make (maxVar) (k1) (k2) (h) (t);
    				end
    			(*Printf.printf "Select Max Variable";*)
    			(*Printf.printf "For All 1 .. m";*)
    		in cycleNodes (m1)
    	in disjunction' (nList) (eList) (List.length nList);;
					
	end;;