
(*module Conjunction = 
	struct*)
		open Syntax
		open Build
		
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
    				| _ -> if var_i > -1 then var_i else -1
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
		
		
		(* This gives a conjunction over bdds (nodes list) given. Takes global dag *)
	(* as input which holds each nodes from nodes list as bdd and returns the  *)
	(* global dag in modified form.                                            *)
	let conjunction (nList) (eList) (h) (t) =
		let rec conjunction' (nodesList) (expList) (m1) =
			let rec cycleNodes m =
				let element = (at m nodesList) in
				if (m1 < 2) then
					begin
						print_string
							("Not enough elements. Node only: \n"^(print_bdd element)^"\n");
						element
					end
				else if (m >1) then
					begin
						match element with
						| Zero -> Zero
						| One -> conjunction' (drop nodesList m) (drop expList m) (m1 -1)
						| _ -> cycleNodes(m -1)
					end
				else
					begin
						match element with
						| Zero -> Zero
						| One -> conjunction' (drop nodesList m) (drop expList m) (m1 -1)
						| _ -> 
							begin
  							let maxVar = select_max_var (nodesList) in
    							if (maxVar !=99999)	then
    								begin
      								let left =
      									get_left_bdd (nodesList) (expList) (maxVar) (h) (t) in
      								let right =
      									get_right_bdd (nodesList) (expList) (maxVar) (h) (t) in
      								let left_exp = get_left_exp (expList) (maxVar) in
      								let right_exp = get_right_exp (expList) (maxVar) in
      								let k1 = conjunction' (left) (left_exp) (List.length left) in
      								let k2 = conjunction' (right) (right_exp) (List.length right) in
      								make (maxVar) (k1) (k2) (h) (t)
    								end
    							else 
										begin 
											Printf.printf "CONJUNCTION: The Max Variable is: %d" maxVar;
											raise (Failure "No more variables left")
										end	 
							end
					end
			in cycleNodes (m1)
		in conjunction' (nList) (eList) (List.length nList)
		
	
	(**)
	(* *)
	(* 
		(* Returns a list of Nodes with new bdd list eval either neg or pos: *)
		(* dependent on left or right *)
    let get_low_high_BDD (nodesList) (expList) (i) (hashtbl3) (h) (t) (leftOrRight: int) = 
    	(*for j=1 to List.length y do*)
			if (leftOrRight=2) then right_ht(hashtbl3) 
			else
				begin
        	Hashtbl.clear hashtbl3;
    			let rec cycleNodes (j) =
        			if (j>(List.length nodesList)) then left_ht(hashtbl3)
        			else
								begin 
  								(*is element labelled by max variable*)
          				let elementX = (at j expList) in
              			begin
              			if (var_bool (Var (string_of_int i)) elementX) then	
              				(
              					print_string ("glh BDD: Variable found.\n");
              					add_to_ht (hashtbl3) 
          							((build (eval(var_lookup (Var (string_of_int i)) 
          								elementX False)) (h) (t) ),
          								(build(eval (var_lookup (Var (string_of_int i)) 
          									elementX True)) (h) (t))) (j);
              					cycleNodes (j+1) 
              				)
              			else
              				(
                				print_string ("glh BDD: Variable in expression not found.\n");
                				add_to_ht (hashtbl3) ((at j nodesList),(at j nodesList)) (j);
                				cycleNodes (j+1)
              				)
              			end
									end
       		in cycleNodes (1)
				end
					
					
		let get_low_high_list_Left (nList1) (eList1) (h) (t) (maxVariable1: int) (temp_ht1) = 
    	let left_list_bdd = get_low_high_BDD (nList1) (eList1) (maxVariable1) (temp_ht1) (h) (t) (1)
    	in left_list_bdd
			
    let get_low_high_list_Right (nList2) (eList2) (h) (t) (maxVariable2: int) (temp_ht1) = 
    	let right_list_bdd = get_low_high_BDD (nList2) (eList2) (maxVariable2) (temp_ht1) (h) (t) (2) 
    	in right_list_bdd
			
    let get_low_high_list_Vals_Left (eList3) (maxVariable3: int) (temp_ht2) = 
    	let left_list_vals = get_low_high_Vals (eList3) (maxVariable3) (temp_ht2) (1)
    	in left_list_vals
			
    let get_low_high_list_Vals_Right (eList4) (maxVariable4: int) (temp_ht2) = 
    	let right_list_vals = get_low_high_Vals (eList4) (maxVariable4) (temp_ht2) (2)		
			in right_list_vals*)
	
(*end;;*)