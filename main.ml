(** The main program. *)
   
	
		open Syntax;;
		open Convert;;
		open Build;;
		open Eval;;
		open Translate;;
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
   
	
	let h = Hashtbl.create 15;;
	let t = Hashtbl.create 15;; 
	
	let global_vars_order_ht_t = Hashtbl.create 15;;
  let global_vars_order_ht_h = Hashtbl.create 15;;
	
	let cycleString st = 
		let rec cycle i = 
			if i> (String.length st)-2 then print_string (st^"\n") else
			if ((String.make 1 st.[i])^(String.make 1 st.[i+1]) = "-c"
				||(String.make 1 st.[i])^(String.make 1 st.[i+1]) = "-d") 
				then begin Printf.printf "QBF_Solver- Option: %s\n" ((String.make 1 st.[i])^(String.make 1 st.[i+1])); 
				cycle (i+1) end
			else cycle (i+1)
		in cycle 0;;

 	let stripOption string option =
      let len = String.length string in
      let rest = String.create len in
      let rec cycle1 i j =
        if i >= len then String.sub rest 0 j
        else if String.contains option string.[i] then
          cycle1 (i+1) (j)
        else begin
          rest.[j] <- string.[i];
          cycle1 (i+1) (j+1)
        end
      in
      cycle1 0 0;;
	
	let processString str =
  let rec cycle st i optionList = 
				if i> (String.length st)-2 then 
					begin 
						print_string (st^"\n"); 
						 (stripOption st "-c-d")::optionList  
					end 
				else
					let first = (String.make 1 st.[i]) in
					let second = (String.make 1 st.[i+1]) in
    			if ((first^second = "-c")||(first^second = "-d")) 
    				then 
							begin 
								Printf.printf "QBF_Solver- Option: %s\n" (first^second); 
    						cycle st (i+1) ((first^second)::optionList)
							end
    			else cycle st (i+1) optionList
  		in cycle str 0 [];;
	
   (** Top level reads input, parses, evaluates and prints the result. *)
   let main =
     print_endline startup ; 
     try
       while true do
         print_string "> ";	
         let str = read_line () in
           try
						let e = str in
						let stringList = processString e in
						Printf.printf "QBF_Solver- You entered: %s \n" e;
						let lenA = List.length stringList in
						if lenA > 1 then (*options available*)
							Printf.printf "QBF_Solver- Options selected: %s \n" (List.nth stringList 1)
						else 
							begin
							Printf.printf "QBF_Solver- Options Not selected.\n";
							let e2 = Parser.toplevel Lexer.lexeme (Lexing.from_string (List.nth stringList 0)) in
  						let e3 = Convert.convertFormula e2 
								(global_vars_order_ht_h) (global_vars_order_ht_t) in
							(*Printf.printf "New Formula: %s\n" (BuildOld.print_exp e3); *)
							let n = Eval.eval e3 in
  						print_endline (string_of_bool n);		
  						if (n) then 
  							begin 
  								try 
  									let m = Build.build e3 (h) (t) in
    								print_endline ("QBF_Solver- "^(Build.print_bdd m));
										print_endline ("QBF_Solver- Old CNF formula: "^(Translate.translate_to_cnf m));
										print_endline ("QBF_Solver- New CNF formula: "^(TranslateINF.translate_to_cnf m))
									with
             			Failure str1 -> print_endline ("QBF_Solver- Error: " ^ str1)
  							end
  						else print_endline "QBF_Solver- The input formula is false\n"
							end 
           with
             Failure str -> print_endline ("QBF_Solver- Error: " ^ str)						
            | Parsing.Parse_error -> print_endline "QBF_Solver- Syntax error."
       done 
     with
       End_of_file -> print_endline "\nQBF_Solver- Good bye."
   ;;

	(*String.contains "-c" "Test-c".[4];;*)
