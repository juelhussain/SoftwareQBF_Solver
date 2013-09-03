module Translate = 
	struct
		
		exception TerminalNode;;

  	(* get variable number of a node *)
      let var (u : int) t = 
        match Hashtbl.find t u with
          | Node(i, _, _) -> i
          | Zero | One -> raise TerminalNode
      ;;
  
  	(* get the left branch of a node *)
      let low (u : int) t = 
        match Hashtbl.find t u with
          | Zero -> Zero
          | One -> One
          | Node(_, l, _) -> l
      ;;
  	
  	(* get the high branch of a node *)
      let high (u : int) t : bdd = 
        match Hashtbl.find t u with
          | Zero -> Zero
          | One -> One
          | Node(_, _, h) -> h
		
		let translate_to_cnf (inputBDD : bdd) = 
			match inputBDD with 
			| One -> One
			| Zero -> Zero
			| Node(x,_,_) -> x 
		
	end;;