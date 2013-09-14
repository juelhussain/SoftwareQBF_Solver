(** The main program. *)
   
	
		open Syntax;;
	

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

	let rec print_list = function 
	[] -> ()
	| e::l -> print_string e ; print_endline " " ; print_list l
	 
   (** Top level reads input, parses, evaluates and prints the result. *)
   let main =
     print_endline startup ; 
     try
       while true do
         print_string "> ";	
         let str = read_line () in
           try
							Printf.printf "Will run the Lexer with %s file\n" str;
							let clauseList = run(open_in str) in
							Printf.printf "There are %d clause in the given file\n" (List.length clauseList);
							print_list (List.rev clauseList)		
           with
             Failure str -> print_endline ("QBF_Solver- Error: " ^ str)						
            | Parsing.Parse_error -> print_endline "QBF_Solver- Syntax error."
						| Sys_error str -> print_endline ("QBF_Solver- System error: " ^ str)
       done 
     with
       End_of_file -> print_endline "\nQBF_Solver- Good bye."
   ;;
	(*String.contains "-c" "Test-c".[4];;*)
