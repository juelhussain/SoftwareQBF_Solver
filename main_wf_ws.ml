(** The main program. *)
   
	
		open Syntax;;
		open Build;;
		open Print;;
		open Convert;;
		open Manager;;
		open TranslateINF;;
	
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


	let rec print_exp_asis (exp) = match exp with 
    | True -> print_string "True"
    | False -> print_string "False"
    | Var x -> Printf.printf "Var %s" x
    | And(x,y) ->  print_string "And ("; print_exp_asis x ;print_string " , "; print_exp_asis y ;print_string ")"
    | Or(x,y) ->  print_string "Or ("; print_exp_asis x ;print_string " , "; print_exp_asis y ;print_string ")"
    | Imp(x,y) ->  print_string "Imp ("; print_exp_asis x ;print_string " , "; print_exp_asis y ;print_string ")"
    | BImp(x,y) ->  print_string "BImp ("; print_exp_asis x ;print_string " , "; print_exp_asis y ;print_string ")"
    | Neg(x) -> print_string "Neg ("; print_exp_asis x; print_string ")"
		| Exists(_,_) -> print_string "exists"
		| Forall(_,_) -> print_string "forall"

	let rec print_exp (exp) = match exp with 
    | True -> print_string "True"
    | False -> print_string "False"
    | Var x -> Printf.printf " %s" x
    | And(x,y) ->  print_exp x ;print_string " /\\tes  "; print_exp y 
    | Or(x,y) -> print_exp x ;print_string " \\/ "; print_exp y 
    | Imp(x,y) ->  print_exp x ;print_string " -> "; print_exp y 
    | BImp(x,y) ->   print_exp x ;print_string " <-> "; print_exp y 
    | Neg(x) -> print_string " ~ "; print_exp x
		| Exists(_,_) -> print_string "exists"
		| Forall(_,_) -> print_string "forall"

	let rec print_list = function 
	[] -> ()
	| e::l -> print_string e ; print_endline " " ; print_list l;;

	let rec print_exp_list = function 
	[] -> ()
	| e::l -> print_exp e ; print_endline " " ; print_exp_list l;;

	let rec print_exp_list_asis = function 
	[] -> ()
	| e::l -> print_exp_asis e ; print_endline " " ; print_exp_list_asis l;;

	
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
							let clauseList = run(open_in str) in
							Printf.printf "QBF_Solver- There are %d clause in the given file\n" (List.length clauseList);
							print_list (List.rev clauseList);
							let expList = Convert.convert_clauses_to_ExpressionList (List.rev clauseList) in
							print_exp_list (expList);
							print_string "---- STARTNIG THE CONJUNCTION PROCESS NOW ----\n\n";
							let exp_size = (List.length expList) in
							Printf.printf "Expression size: %d\n" exp_size;
							let segment_val = 2
							(*(exp_size/(if (exp_size > 100) then exp_size/20 
								else if (exp_size >50) then exp_size/10 else exp_size/2))*) 
								in
								Printf.printf "QBF_Solver- Segmentation value: %d\n" segment_val;
							let con_bdd_list = Manager.start_process_segmentation (expList) (h) (t) (segment_val) 
							in
							let cnf_con_list = TranslateINF.translate_list_to_cnf(con_bdd_list)
							in
							let cnf_list = TranslateINF.to_cnf_list (cnf_con_list)
							in
							let qdimacs = Print.get_exp_list_to_qdimacs(cnf_list) in
							Printf.printf "QBF_Solver- The final conjunction obdds: \n%s\n\n" (Print.get_print_bdd_list (con_bdd_list));
							print_string"-------------------------CONJUNCTION FORMULA---------------------------------------\n\n";	
							Printf.printf "%s\n" (Print.get_print_exp_list (cnf_con_list));
							Printf.printf "QBF_Solver- segmentation value: %s\n" (string_of_int segment_val);
							print_string"---------------------------QDIMACS FORMAT------------------------------------------\n\n";	
							Printf.printf "%s\n%s\n" ("p cnf "^(string_of_int exp_size)) (qdimacs);						
           with
             Failure str -> print_endline ("QBF_Solver- Error: " ^ str)						
            | Parsing.Parse_error -> print_endline "QBF_Solver- Syntax error."
						| Sys_error str -> print_endline ("QBF_Solver- System error: " ^ str)
       done 
     with
       End_of_file -> print_endline "\nQBF_Solver- Good bye."
   ;;
	(*String.contains "-c" "Test-c".[4];;*)
