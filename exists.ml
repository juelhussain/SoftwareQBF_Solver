
module Exists =
struct
	
	let checkList listV var =
		let rec list1 i =
			if (i > (List.length listV) - 1) then
				false
			else
				begin
					if var = List.nth listV i then true
					else list1 (i +1)
				end
		in list1 (0)
	
	let checkListNo listV var =
		let rec list1 i =
			if (i > (List.length listV) - 1) then
				-1
			else
				begin
					if var = List.nth listV i then i +1
					else list1 (i +1)
				end
		in list1 (0)
	
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
		
		let get_left_exp (expList) (i) = 
    	let rec left newlist j =
    		if (j>(List.length expList)) then (List.rev newlist)
    		else 
    			begin
    				let element_exp = (at j expList) in
    					begin
          			if (var_bool (Var (string_of_int i)) element_exp) then	
          				begin
          					print_string ("get_left_exp: Variable found.\n");
    								left ((eval(var_lookup (Var (string_of_int i)) element_exp False))::newlist) (j+1)
          				end
          			else
          				begin
            				print_string ("get_left_exp: Variable in expression not found.\n");
            				left (element_exp::newlist) (j+1)
          				end
        			end
    			end
    	in left [] 1 ;;
    
    let get_right_exp (expList) (i) = 
    	let rec left newlist j =
    		if (j>(List.length expList)) then (List.rev newlist)
    		else 
    			begin
    				let element_exp = (at j expList) in
    					begin
          			if (var_bool (Var (string_of_int i)) element_exp) then	
          				begin
          					print_string ("get_right_exp: Variable found.\n");
    								left ((eval(var_lookup (Var (string_of_int i)) element_exp True))::newlist) (j+1)
          				end
          			else
          				begin
            				print_string ("get_right_exp: Variable in expression not found.\n");
            				left (element_exp::newlist) (j+1)
          				end
        			end
    			end
    	in left [] 1 ;;
    
    
    
    let get_left_bdd (nodesList) (expList) (i) (h) (t) = 
    	let rec left newlist j =
    		if (j>(List.length expList)) then (List.rev newlist)
    		else 
    			begin
    				let element_exp = (at j expList) in
    					begin
          			if (var_bool (Var (string_of_int i)) element_exp) then	
          				begin
          					print_string ("get_left_bdd: Variable found.\n");
    								 left ((build(eval(var_lookup (Var (string_of_int i)) element_exp False)) (h) (t) )::newlist) (j+1)
          				end
          			else
          				begin
            				print_string ("get_left_bdd: Variable in expression not found.\n");
            				left ((at j nodesList)::newlist) (j+1)
          				end
        			end
    			end
    	in left [] 1 ;;
    
    let get_right_bdd (nodesList) (expList) (i) (h) (t) = 
    	let rec left newlist j =
    		if (j>(List.length expList)) then (List.rev newlist)
    		else 
    			begin
    				let element_exp = (at j expList) in
    					begin
          			if (var_bool (Var (string_of_int i)) element_exp) then	
          				begin
          					print_string ("get_right_bdd: Variable found.\n");
    								 left ((build(eval(var_lookup (Var (string_of_int i)) element_exp True)) (h) (t) )::newlist) (j+1)
          				end
          			else
          				begin
            				print_string ("get_right_bdd: Variable in expression not found.\n");
            				left ((at j nodesList)::newlist) (j+1)
          				end
        			end
    			end
    	in left [] 1 ;;
		
		
		
			
		(* selects max variable in nodeslist. Lower value takes presedence.*)
		(* Also only first node is checked. Nested nodes are ignored - *)
		(* may have to revise *)
		let select_max_var (nodesList) =
    	let rec iter element i var_i= 
    		if (i >= (List.length nodesList)) then begin 
    		let max = match element with	
    				| Node(x,_,_) -> if ((x < var_i) && not(x<0)) then x else var_i
    				| _ -> if (var_i > 0) && (var_i != 99999) then var_i else raise (Failure "There are no max variables")
						in Printf.printf "Max variable: %d\n" max ; max
    		 end 
    			else
    				match element with	
    				| Node(x,_,_) -> 
    					if ((x < var_i) && not(x<0)) then 
    						begin
      						(iter (at (i+1) nodesList) (i+1) (x)) 
    						end
    					else 
    						begin 
      						(iter (at (i+1) nodesList) (i+1) (var_i)) 
    						end
    				| _ -> iter (at (i+1) nodesList) (i+1) var_i
    in iter (at 1 nodesList) (1) (99999)	
	
	
	
	
	(* This gives a existential quantification over bdds (nodes list) and      *)
	(* quantified variables list given. Takes global dag as input which holds  *)
	(* each nodes from nodes list as bdd and returns the global dag in         *)
	(* modified form.                                                          *)
	
	let exists (vList) (nList) (eList) (h) (t) =
		let rec exists' (varList) (nodesList) (expList) (m1) =
			if (m1 < 1) then
					begin
						Zero
					end 
			else if((List.length nodesList) != (List.length expList)) then 
				raise (Failure "The expression list and nodes list does not match")
			else 
				begin
    			let rec cycleNodes m =
    				let element = (at m nodesList) in
    				if (m >1) then
    					begin
    						match element with
    						| One -> One
    						| Zero -> exists' (varList) (drop nodesList m) (drop expList m) (m1 -1)
    						| _ -> cycleNodes(m -1)
    					end
    				else
    					begin
    						match element with
    						| One -> One
    						| Zero -> exists' (varList) (drop nodesList m) (drop expList m) (m1 -1)
    						| _ -> 
    							begin
      							let maxVar = select_max_var (nodesList) in
    								let left =
    									get_left_bdd (nodesList) (expList) (maxVar) (h) (t) in
    								let right =
    									get_right_bdd (nodesList) (expList) (maxVar) (h) (t) in
    								let leftX = get_left_exp (expList) (maxVar) in
    								let rightX = get_right_exp (expList) (maxVar) in
										if (checkList varList maxVar) then
											(
												exists' (drop varList (checkListNo varList maxVar)) (left@right) (leftX@rightX)
												((List.length left) + (List.length right))
											)
										else
											(
    										let k1 = exists' (varList) (left) (leftX) (List.length left) in
    										let k2 = exists' (varList) (right) (rightX) (List.length right) in
    										make (maxVar) (k1) (k2) (h) (t)
											)
        					end
    					end
    			in cycleNodes (m1)
  			end
		in exists' (vList) (nList) (eList) (List.length nList)
	
	
end;;