module Manager = struct
	
	
	(* 1/ This is the entry point to the full evaluation process *)
	let start = ();;
	
	(* 2/ This where the expression list will be afforded. *)
	(* Followed by correct data representation for each clause. *)
	let check_clause = ();;

	(* 3/ For each of the clauses the build function will be run *)
	(* with the OBDDs for each data stored in the Hashtable. *)
	build_clauses = ();;

	(* 4/ For the clauses (now with OBDDs available) the conjunction *)
	(* module will be run. *)
	let conjunction_clauses = ();;
	
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

	end;;