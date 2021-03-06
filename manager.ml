(*module Manager = struct*)
	
	open Syntax;;
	open Build;;
	(*open Operations;;*)
	open Print;;
	open Conjunction;;
	open Disjunction;; 
	open Convert
	
	
	
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

	let is_quant el = 
			match el with 
			| Exists(_,_) -> true
			| Forall(_,_) -> true
			| _ -> false
		
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

	let check_clause_size (exp) = 
		let rec nextcheck var i =
			match var with
			| True -> i
			| False -> i
			| Var x -> i
			| Neg(_) -> i
			| Exists(_,_) -> i
			| Forall(_,_) -> i
			| And(x,y) -> (nextcheck x (i+1))+(nextcheck y (i+1)) -1
			| Or(x,y) -> (nextcheck x (i+1))+(nextcheck y (i+1)) -1
			| Imp(x,y) -> (nextcheck x (i+1))+(nextcheck y (i+1)) -1
			| BImp(x,y) -> (nextcheck x (i+1))+(nextcheck y (i+1)) -1
		in nextcheck exp 0

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
					else (raise (Failure ("manager:The clause is not in correct format: "^(get_print_exp_asis clause))))
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
		if (segment_val>=(List.length exp_list)) then [] (*raise (Failure "List is too small")*)
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
		
		
		let rec build_string = function 
      	[] -> ""
      	| h::t -> h^" "^(build_string t)


		let get_clause_disjunct string_clause break_up_no h t = 
			 (* A string clause more than break_up_no variables is going to come in*)
			let string_clause_list = split ' ' string_clause in
			let rec process dis_bdd_list dis_exp_list i c_list =
				if ((i+break_up_no*2)<=(List.length c_list)) then 
				 begin
						(*get the partial string up to amount*)
						let new_clause = build_string (get_reduced_list c_list break_up_no) in
  					let c_list = reduce_list c_list break_up_no in
  					let new_exp = convert_clause_to_Expression (new_clause) in
						let bdd1 = build_clause (new_exp) (h) (t) in
      			
						(*get second partial list*)
      			let new_clause2 = build_string (get_reduced_list c_list break_up_no) in
      			let c_list = reduce_list c_list break_up_no in
      			let new_exp2 = convert_clause_to_Expression (new_clause2) in
    				let bdd2 = build_clause (new_exp2) (h) (t) in
          					
          	(* Get this disjunction of the two*)
          	let new_dis_bdd = disjunction ([bdd1;bdd2]) ([new_exp;new_exp2]) (h) (t)
          	in
          	process (new_dis_bdd::dis_bdd_list) ((Or(new_exp,new_exp2))::dis_exp_list) (i+break_up_no*2) c_list
					end	
				else if ((i=1)&&((i+break_up_no*2)>=(List.length c_list)))
				then 
					begin
						let new_clause3 = build_string (c_list) in
						let new_exp3 = convert_clause_to_Expression (new_clause3) in
						let bdd3 = build_clause (new_exp3) (h) (t) in
								(* Get this disjunction of everything*)
						bdd3
					end
				else
					begin
						let new_clause3 = build_string (c_list) in
						let new_exp3 = convert_clause_to_Expression (new_clause3) in
						let bdd3 = build_clause (new_exp3) (h) (t) in	
						(* Get this disjunction of everything*)
						let new_dis_bdd = disjunction (bdd3::dis_bdd_list) (new_exp3::dis_exp_list) (h) (t)
						in
						new_dis_bdd
					end
				in process [] [] 1 string_clause_list
		
		(* Newest design. Like segmentation this will break up the incoming conjunction. *)
		(* But this also tackeles the clause it self, if there are many variables, to *)
		(* avoid exponential break up will handle in smaller chunks.*)
		
		let start_process_segmentation_double stringClauseList (h) (t) (seg_val) (segment_or)=
			let rec segment conjunction_bdd_list exp_list bdd_list i j =
				if (i>=(List.length stringClauseList)) then 
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
					begin
						(* First check if there are more than 4 variables in a clause. *)
						(* If there is, then to avoid exponential blow up and processing time,*)
						(* break up the building by five varibles each turn until last clause which *)
						(* should be 4 or less. Then run disjunction between them. *)
  					let string_clause = (List.nth stringClauseList i) in
						let clause = convert_clause_to_Expression (string_clause) in
						if (is_quant(clause)) then 
							segment conjunction_bdd_list (exp_list) (bdd_list) (i+1) (j)
  					else if ((List.length (split ' ' string_clause))>(segment_or*2)) then 
							begin
							print_endline "manager: segmenting disjunction";
  						let clause_bdd = (get_clause_disjunct string_clause segment_or h t) in 
							segment conjunction_bdd_list 
  								((convert_clause_to_Expression string_clause)::exp_list) (clause_bdd::bdd_list) (i+1) (j+1)
  						end
						else 
							let clause = convert_clause_to_Expression (List.nth stringClauseList i) in
							if (check_clause clause) then 
  							begin
  								(*build the clause*)
  								segment conjunction_bdd_list 
  									(clause::exp_list) ((build_clause (clause) (h) (t))::bdd_list) (i+1) (j+1);
  							end
  						else (raise (Failure ("manager:The clause is not in correct format: "^(Print.get_print_exp_asis clause))))
					end
				in segment [] [] [] 0 0;;
	
		
		(*let start_process_segmentation_with_grouping expressionList (h) (t) (seg_val) =
			let expressionList = remove_quants (expressionList) in
			(*Varibale grouping here to genrate a new expression list with *)
			(* clauses in order of same variables in frequency number. For example*)
			(* if original clauses was: 1 2 3; 4 5 6; 3 2 1; 6 5 4; rather than running conjunction *)
			(* with 1 and 2 which has different variables (so large OBDD) we run it with 1 and 3 *)
			(* and 2 and 4. Resulting in much smaller OBDDs especially if they were negation *)
			(* varables*)
			let expressionList = order_clauses (expressionList) in			
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
				in segment [] [] [] 0 0;;*)

		
					
					
					
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