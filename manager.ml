(*module Manager = struct*)
	
	open Syntax;;
	open Build;;
	(*open Operations;;*)
	open Print;;
	open Conjunction;;
	
	
	
	
	(* 1a/ This will remove the quantifiers from the expression list and saved *)
	(* for later.*)
	let remove_quants el = 
		let rec elems el2 i = 
			if (i>(List.length el)-1) then (List.rev el2) else
			let element = (List.nth el i) in 
			match element with 
			| Exists(_,_) -> elems el2 (i+1)
			| Forall(_,_) -> elems el2 (i+1)
			| _ -> elems (element::el2) (i+1)
		in elems [] 0;;
		
	(* 1b/ This will get the quantifiers from the expression list as a *)
	(* separate list*)
	let sep_quants el = 
		let rec elems el2 i = 
			if (i>(List.length el)-1) then (List.rev el2) else
			let element = (List.nth el i) in 
			match element with 
			| Exists(_,_) -> elems (element::el2) (i+1)
			| Forall(_,_) -> elems (element::el2) (i+1)
			| _ -> elems (el2) (i+1)
		in elems [] 0;;
	
	(* 2/ This where the expression list will be afforded. *)
	(* Followed by correct data representation for each clause. *)
	
	let rec check_clause exp = match exp with 
	| True -> true
	| False -> true
	| Var y -> true
	| And(x,y) -> check_clause (x) & check_clause (y)
	| Or(x,y) -> check_clause (x) & check_clause (y)
	| Imp(_,_) -> false
	| BImp(_,_) -> false
	| Forall(_,_) -> false
	| Exists (_,_) -> false
	| Neg (x) -> check_clause (x)

	(* 3/ For each of the clauses the build function will be run *)
	(* with the OBDDs for each data stored in the Hashtable. *)
	let build_clause clause (h) (t) = 
		let bdd = build (clause) (h) (t) in bdd;;

	(* 4/ For the clauses (now with OBDDs available) the conjunction *)
	(* module will be run. *)
	let conjunction_clauses expression_list bdd_list h t = 
		(*Printf.printf "conjunction_clauses: The size of expression list: %d \n obdd list: %d\n" 
			(List.length expression_list) (List.length bdd_list);*)
		let con_bdd = conjunction (bdd_list) (expression_list) (h) (t) in
		Printf.printf "\n"; 		
		con_bdd;;
	
	(* 1/ This is the entry point to the full evaluation process *)
	let start_process expressionList (h) (t) = 
		let expressionList = remove_quants (expressionList) in
		let rec build_exp exp_list bdd_list i =
			if (i>(List.length expressionList)-1) then 
				(*return the conjunction of obdd_list *)
				begin
					let bdd_list = (List.rev bdd_list) in
					Printf.printf "start_process: The input lists sizes are: exp: %d bdd: %d\n" (List.length exp_list) (List.length bdd_list);
					(*Printf.printf "The contents of bdd: \n"; print_bdd_list bdd_list;
					Printf.printf "The contents of exp: \n"; print_exp_list exp_list;*)
					let conjunction_bdd = 
					conjunction_clauses (exp_list) (bdd_list) (h) (t) in 
					conjunction_bdd
				end
			else
				begin
					(*check the expression is correct format*)
					let clause = (List.nth exp_list i) in 
					if (check_clause clause) then 
						begin
							(*build the clause*)
							build_exp exp_list ((build_clause (clause) (h) (t))::bdd_list) (i+1);
						end
					else (raise (Failure ("The clause is not in correct format: "^(get_print_exp_asis clause))))
				end	
		in build_exp expressionList [] 0;;
		
		(* This is the part of the conjunction process that will use segmentation.*)
	(* First the size of the nodes list is checked and that will be devided by a *)
	(* given value. At each break point the conjunction process will be run and *)
	(* the conjunction_bdd for each run will be provided in a list form where*)
	(* the elements have implicit conjunction between them. *)
	
	
	let get_reduced_list exp_list segment_val = 
		(*Valdation check*)
		if (segment_val>=(List.length exp_list)) then exp_list (*raise (Failure "List is too small")*)
		else
			begin
    		let rec segm new_exp_list i = 
    			if (i>=segment_val) then 
    				(List.rev new_exp_list)
    			else
    				begin
    					let element = List.nth exp_list i in
    					segm (element::new_exp_list) (i+1)
    				end
    			in segm [] 0
			end
			;;

	let reduce_list exp_list segment_val = 
		(*Valdation check*)
		if (segment_val>=(List.length exp_list)) then exp_list (*raise (Failure "List is too small")*)
		else
			begin
    		let rec reduce new_exp_list i = 
    			if (i<=segment_val) then 
    				(new_exp_list)
    			else
    				begin
    					let element = List.nth exp_list (i-1) in
    					reduce (element::new_exp_list) (i-1)
    				end
    			in reduce [] (List.length exp_list)
			end
			;;
	
	let segment_conjunction (expression_list) (obdd_list) (h) (t) (segment_val) = 
		let rec segment (exp_list) (b_list) (conjunction_bdd_list) i=
			if (i>((List.length expression_list) / segment_val)) then List.rev conjunction_bdd_list
			else
				begin
					if (segment_val < (List.length b_list)) then 
  					(
  						let conjunction_bdd = 
    					conjunction_clauses (get_reduced_list exp_list segment_val) 
							(get_reduced_list b_list segment_val) (h) (t) in 
    					segment (reduce_list exp_list segment_val) (reduce_list b_list segment_val) 
							(conjunction_bdd::conjunction_bdd_list) (i+1)
						)
					else 
						(
  						let conjunction_bdd = 
    					conjunction_clauses (exp_list) (b_list) (h) (t) in 
    					segment (exp_list) (b_list) (conjunction_bdd::conjunction_bdd_list) (i+1)
						)
				end
			in segment (expression_list) (obdd_list) [] 0 ;;
		
		
		(* New design. Take the expression list and break it up according to segmentation value. *)
		(* Then build the expressions and each time a breakup happens create a new Hashtbl set. *)
		(* Then pass those as parameters to conjunction_bdd. *)
		let start_process_segmentation expressionList (h) (t) (seg_val) =
			let expressionList = remove_quants (expressionList) in
			let rec segment conjunction_bdd_list exp_list bdd_list i j =
				if (i>=(List.length expressionList)) then 
					if (j=0) then List.rev (conjunction_bdd_list)
					else
						begin
    					let conjunction_bdd = conjunction_clauses (exp_list) (bdd_list) (h) (t)
    					in List.rev (conjunction_bdd::conjunction_bdd_list)
						end
				else if (j>=seg_val) then 
					begin
						(*CALL CONJUNCTION WITH bdd_list and exp_list and then reset*)
						let conjunction_bdd = 
							conjunction_clauses (exp_list) (bdd_list) (h) (t) in 						
						let exp_list = [] in
						let bdd_list = [] in
						Hashtbl.clear h;
						Hashtbl.clear t;
						segment (conjunction_bdd::conjunction_bdd_list) exp_list bdd_list i 0
					end
				(* When j is segment value reset. Keep recursion until i = expressionList *)
				else
				let clause = (List.nth expressionList i) in 
					if (check_clause clause) then 
						begin
							(*build the clause*)
							segment conjunction_bdd_list 
								(clause::exp_list) ((build_clause (clause) (h) (t))::bdd_list) (i+1) (j+1);
						end
					else (raise (Failure "The clause is not in correct format"))
				in segment [] [] [] 0 0;;
						
		let start_process_segmentation_Old2 expressionList (h) (t) (seg_val) =
			let expressionList = remove_quants (expressionList) in
			let rec segment conjunction_bdd_list exp_list bdd_list h t i j =
				if (i>=(List.length expressionList)) then 
					if (j=0) then List.rev (conjunction_bdd_list)
					else
						begin
    					let conjunction_bdd = conjunction_clauses (exp_list) (bdd_list) (h) (t)
    					in List.rev (conjunction_bdd::conjunction_bdd_list)
						end
				else if (j>=seg_val) then 
					begin
						(*CALL CONJUNCTION WITH bdd_list and exp_list and then reset*)
						let conjunction_bdd = 
							conjunction_clauses (exp_list) (bdd_list) (h) (t) in 						
						let exp_list = [] in
						let bdd_list = [] in
						let h = Hashtbl.create 15 in
						let t = Hashtbl.create 15 in
						segment (conjunction_bdd::conjunction_bdd_list) exp_list bdd_list h t i 0
					end
				(* When j is segment value reset. Keep recursion until i = expressionList *)
				else
				let clause = (List.nth expressionList i) in 
					if (check_clause clause) then 
						begin
							(*build the clause*)
							segment conjunction_bdd_list 
								(clause::exp_list) ((build_clause (clause) (h) (t))::bdd_list) h t (i+1) (j+1);
						end
					else (raise (Failure "The clause is not in correct format"))
				in segment [] [] [] h t 0 0;;
						
						
		(*This is to be called for conjunction bdd with segmentation for optimisation*)
		let start_process_segmentation_old expressionList (h) (t) (seg_val) = 
		let expressionList = remove_quants (expressionList) in
		let rec build_exp exp_list bdd_list i =
			if (i>(List.length expressionList)-1) then 
				(*return the conjunction of obdd_list *)
				begin
					let bdd_list = List.rev bdd_list in
					Printf.printf "The input lists sizes are: exp: %d bdd: %d\n" (List.length exp_list) (List.length bdd_list);
					Printf.printf "The contents of bdd: \n"; print_bdd_list bdd_list;
					Printf.printf "The contents of exp: \n"; print_exp_list exp_list;
					let conjunction_bdd_list = 
					segment_conjunction (exp_list) (bdd_list) (h) (t) (if (seg_val < 2) then 2 else seg_val) in 
					conjunction_bdd_list
				end
			else
				begin
					(*check the expression is correct format*)
					let clause = (List.nth exp_list i) in 
					if (check_clause clause) then 
						begin
							(*build the clause*)
							build_exp exp_list ((build_clause (clause) (h) (t))::bdd_list) (i+1);
						end
					else (raise (Failure "The clause is not in correct format"))
				end	
		in build_exp expressionList [] 0;;
	
	
	
	
	
	(* 5/ Take the conjunction obdd and present the printout of the OBDD. *)
	let get_conjunction_obdd = ();;

	(* 6/ Convert OBDD to simplified QBF. *)
	let convert_obdd_to_cnf = ();;

	(* 7/ Apply quantifiers for full simplified qbf *)
	let simplified_qbf = ();;

	(* 8/ Run the qbf2epr with simplified QBF*)
	let run_qbf2epr = ();;

	(* 9/ Run iProver with epr formula*)
	let run_iprover = ();;

	(* 10/ Return iprover stats along with time for solving.*)
	(* This will include timing for wach individual section breakdown. *)
	let get_results = ();;


	


(*	end;;*)