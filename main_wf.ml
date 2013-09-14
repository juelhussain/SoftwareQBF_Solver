(** The main program. *)
   
	
		open Syntax;;
		open Clauses;;

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
   
	
	 
   (** Top level reads input, parses, evaluates and prints the result. *)
   let main =
     print_endline startup ; 
     try
       while true do
         print_string "> ";	
         let str = read_line () in
           try
						let e = Parser_dimacs.main Lexer_dimacs.token (Lexing.from_string str)
						in
  						print_endline (Clauses.clause_to_string(Clauses.getClause1(Clauses.getClause1(e))));		
           with
             Failure str -> print_endline ("QBF_Solver- Error: " ^ str)						
            | Parsing.Parse_error -> print_endline "QBF_Solver- Syntax error."
       done 
     with
       End_of_file -> print_endline "\nQBF_Solver- Good bye."
   ;;

	(*String.contains "-c" "Test-c".[4];;*)
