(** The main program. *)
   
	
		open Syntax;;
		open Build;;
		open Print;;
		open Convert;;
		open Manager;;
		open TranslateINF;;
		open Write;;

 
   (** The end of file character. *)
   let eof =
     match Sys.os_type with
         "Unix" | "Cygwin" -> "Ctrl-D"
       | "Win32" -> "Ctrl-Z"
       | _ -> "\"end of file\""
   ;;
   
	
	
   (** The startup message. *)
   let startup = "QBF Solver. Press " ^ eof ^ " to quit."
   ;;
   
	
	
	let run x = let lb =
	Lexing.from_channel x in
	Parser_dm.main (Lexer_dm.tokens) lb	;;

	
	let get_var_num clause_list = 
		if (List.length clause_list) > 0 then 
		begin
			let header = List.nth clause_list 0 in
			if ((String.sub header 0 1) = "p") then
				begin 
					(List.nth (Convert.split ' ' (header)) 2)^" "
				end
			else "NA "
		end
		else raise (Failure "get_var_num: given clause_list is empty")
		
		
		
   (** Top level reads input, parses, evaluates and prints the result. *)
   let main =
     print_endline startup ; 
     try
       while true do
         print_string "> ";	
         let str = read_line () in
           try
							let h = Hashtbl.create 15 in
							let t = Hashtbl.create 15 in
							Printf.printf "QBF_Solver- Will run the Lexer with %s file\n" str;
							let ic = open_in str in
							let clauseList = Syntax.time1 ("parsing (file: "^str^")") (run) (ic) in
							let clauseList=List.rev clauseList in
							close_in ic;
							(*Printf.printf "QBF_Solver- There are %d clause in the given file\n" (List.length clauseList);*)
							print_endline "Input file header:";
							print_string (List.nth clauseList 0);
							(*print_endline "";*)
							let expList = Syntax.time1 "Conversion to Expression" Convert.convert_clauses_to_ExpressionList (clauseList) in
							let quant_list = Manager.sep_quants expList in
							(*Print.print_exp_list (expList);*)
							print_string "---- STARTNIG THE CONJUNCTION PROCESS NOW ----\n\n";
							let exp_size = (List.length expList) in
							Printf.printf "Expression size: %d\n" exp_size;
							let segment_val = 2
							(*(exp_size/(if (exp_size > 100) then exp_size/20 
								else if (exp_size >50) then exp_size/10 else exp_size/2))*) 
								in
							let con_bdd_list = Syntax.time4 "Process" Manager.start_process_segmentation (expList) (h) (t) (segment_val) 
							in
							let cnf_con_list = Syntax.time1 "Translation to INF" TranslateINF.translate_list_to_cnf (con_bdd_list)
							in
							let cnf_list = Syntax.time1 "Translation to CNF" TranslateINF.to_cnf_list (cnf_con_list)
							in
							let qdimacs = Syntax.time1 "QDIMACS" Print.get_exp_list_to_qdimacs (quant_list@cnf_list) in
							(*Printf.printf "QBF_Solver- The final conjunction obdds: \n%s\n\n" (Print.get_print_bdd_list (con_bdd_list));*)
							(*print_string"-------------------------CONJUNCTION FORMULA---------------------------------------\n\n";	
							Printf.printf "%s\n" (Print.get_print_exp_list (cnf_con_list));*)
							let header = "p cnf "^(get_var_num (clauseList))^(string_of_int(Write.get_number_of_clauses (Convert.split '\n' qdimacs) - (List.length quant_list)))^"\n" in
							Printf.printf "QBF_Solver- segmentation value: %s\n" (string_of_int segment_val);
							print_string"---------------------------QDIMACS FORMAT------------------------------------------\n\n";	
							Write.write ("output_files/"^(String.sub str 6 ((String.length str)-14))^"_QBFsolver.qdimacs" ) (header^(qdimacs));
							(*Printf.printf "%s\n"(qdimacs);	*)				
           with
             Failure str -> print_endline ("QBF_Solver- Error: " ^ str)						
            | Parsing.Parse_error -> print_endline "QBF_Solver- Syntax error."
						| Sys_error str -> print_endline ("QBF_Solver- System error: " ^ str)
       done 
     with
       End_of_file -> print_endline "\nQBF_Solver- Good bye."
   ;;
	(*String.contains "-c" "Test-c".[4];;*)
