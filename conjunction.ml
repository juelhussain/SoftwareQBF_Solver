
(*module Conjunction = 
	struct*)
		open Syntax
		open Build
		
		let counter =
  		let count = ref (-1) in
  			fun () -> incr count; !count
		
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
          					(*print_string ("get_left_exp: Variable found.\n");*)
    								left ((eval(var_lookup (Var (string_of_int i)) element_exp False))::newlist) (j+1)
          				end
          			else
          				begin
            				(*print_string ("get_left_exp: Variable in expression not found.\n");*)
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
          					(*print_string ("get_right_exp: Variable found.\n");*)
    								left ((eval(var_lookup (Var (string_of_int i)) element_exp True))::newlist) (j+1)
          				end
          			else
          				begin
            				(*print_string ("get_right_exp: Variable in expression not found.\n");*)
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
          					(*print_string ("get_left_bdd: Variable found.\n");*)
    								 left ((build(eval(var_lookup (Var (string_of_int i)) element_exp False)) (h) (t) )::newlist) (j+1)
          				end
          			else
          				begin
            				(*print_string ("get_left_bdd: Variable in expression not found.\n");*)
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
          					(*print_string ("get_right_bdd: Variable found.\n");*)
    								 left ((build(eval(var_lookup (Var (string_of_int i)) element_exp True)) (h) (t) )::newlist) (j+1)
          				end
          			else
          				begin
            				(*print_string ("get_right_bdd: Variable in expression not found.\n");*)
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
						in (*Printf.printf "Max variable: %d\n" max ;*) max
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
	
	(* Example: conjunction([Node(1,Zero,One);Node(2,Zero,One);Node(3,One,Zero)])*)
	(* ([Var "1";Var "2";Neg(Var "3")]) (Hashtbl.create 15) (Hashtbl.create 15);; *)
	
	let conjunction (nList) (eList) (h) (t) =
		(*Validation check*)
		let node_check = (List.nth nList 0) in
		let exp_check = (build (List.nth eList 0) (h) (t)) in
		let node_check2 = (List.nth nList ((List.length nList)-1)) in
		let exp_check2 = (build (List.nth eList ((List.length eList)-1)) (h) (t)) in
		if (not((node_check=exp_check)&&(node_check2=exp_check2))) then 
			raise (Failure "The obdds and formulas don't match")
		else 
			begin
				let counter =
      		let count = ref (-1) in
      			fun () -> incr count; !count in
    		let rec conjunction' (nodesList) (expList) (m1) =
    			Printf.printf "%d " (counter());
    			if (m1 < 2) then
    					begin
    						let element = (at m1 nodesList) in
    						(*print_string ("Not enough elements. Node only: \n"^(print_bdd element)^"\n");*)
    						element
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
        								let left_bdd =
        									get_left_bdd (nodesList) (expList) (maxVar) (h) (t) in
        								let right_bdd =
        									get_right_bdd (nodesList) (expList) (maxVar) (h) (t) in
        								let left_exp = get_left_exp (expList) (maxVar) in
        								let right_exp = get_right_exp (expList) (maxVar) in
        								let k1 = conjunction' (left_bdd) (left_exp) (List.length left_bdd) in
        								let k2 = conjunction' (right_bdd) (right_exp) (List.length right_bdd) in
        								make (maxVar) (k1) (k2) (h) (t)
            					end
        					end
        			in cycleNodes (m1)
      			end
    		in conjunction' (nList) (eList) (List.length nList)
  		end;;
	
(*end;;*)