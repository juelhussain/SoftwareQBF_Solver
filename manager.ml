module Manager = struct
	
	open Build;;

	(* 1/ This is the entry point to the full evaluation process *)
	let start expressionList (h) (t) = 
		let rec build_exp exp_list bdd_list i =
			if (i>(List.length expressionList)-1) then 
				(*return the conjunction of obdd_list *)
				begin
				let conjunction_bdd = 
					conjunction_clauses (exp_list) (bdd_list) (h) (t) in 
					conjunction_bdd
				end
			else
				begin
					(*check the expression is correct format*)
					let clause = (List.nth exp_list i) in 
					if (check_clause exp) then 
						begin
							(*build the clause*)
							let bdd = build_clause (clause) (h) (t) in 
							build_exp exp_list (bdd::bdd_list) (i+1);
						end
					else (raise (Failure "The clause is not in correct format"))
				end	
		in build_exp expressionList [] 0;;
	
	
	
	(* 2/ This where the expression list will be afforded. *)
	(* Followed by correct data representation for each clause. *)
	let check_clause exp = true;;

	(* 3/ For each of the clauses the build function will be run *)
	(* with the OBDDs for each data stored in the Hashtable. *)
	let build_clause clause (h) (t) = 
		let bdd = build (clause) (h) (t) in bdd;;

	(* 4/ For the clauses (now with OBDDs available) the conjunction *)
	(* module will be run. *)
	let conjunction_clauses expression_list bdd_list h t = 
		Printf.printf "the size of expression list: %d \n obdd list: %d\n" 
			(List.length expression_list) (List.length bdd_list);
		"will run conjunction module";;
	
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


	(* 1/ This is the entry point to the full evaluation process *)
	let startOLD expressionList (h) (t) = 
		let rec build_exp exp_list bdd_list i =
			if (i>(List.length expressionList)-1) then 
				(*return the bdd list *)
				bdd_list
				(*check the expression is correct format*)
			else
				begin
					let clause = (List.nth exp_list i) in 
					if (check_clause exp) then 
						begin
							(*build the clause*)
							let bdd = build_clause (clause) (h) (t) in 
							build_exp exp_list (bdd::bdd_list) (i+1);
						end
					else (raise (Failure "The clause is not in correct format"))
				end	
			in build_exp expressionList [] 0;;
	


	end;;