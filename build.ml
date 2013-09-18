(*module Build = 
	struct*)
	
	open Syntax;;
	
	
	exception TerminalNode
	
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
    ;;

	(* get the evaluation of an expression. If the expression can not be *)
	(* completely reduced to true or false then the expression is returned *)
	(* unaltered*)

 let rec eval (exp) = 
      match exp with
				| Forall(_,y) -> eval y
				| Exists (_,y) -> eval y 
        | True -> True
        | False -> False
        | Var i -> Var i
        | Neg x -> (
            match x with
              | True -> False
              | False -> True
              | _ -> Neg (eval x)
          )
        | And(x,y) -> (
            match (x,y) with
              | (True,True) -> True
              | (True,False) -> False
              | (False, True) -> False
              | (False, False) -> False
              | (_, _) -> And(eval x, eval y)
          )
        | Or(x,y) ->  (
            match (x,y) with 
              | (True,True) -> True
              | (True, False) -> True
              | (False, True) -> True
              | (False, False) -> False
              | (_, _) -> Or(eval x,eval y)
          )
        | Imp(x,y) -> (
            match (x,y) with 
              | (True,True) -> True 
              | (True,False) -> False
              | (False, True) -> True
              | (False, False) -> True
              | (_, _) -> Imp(eval x,eval y)
          )
        | BImp(x,y) -> (
            match (x,y) with
              | (True,True) -> True
              | (True,False) -> False 
              | (False, True) -> False
              | (False, False) -> True
              | (_, _) -> BImp(eval x,eval y)
          )
	
	(* check if a variable exists in the current expression *)
    let rec var_bool (v : expression) (e : expression) : bool = 
      match e with
        | True -> false
        | False -> false
        | Var i -> let Var j = v in j = i (* important part *)
        | And(x, y) -> var_bool v x || var_bool v y
        | Or(x, y) -> var_bool v x || var_bool v y
        | BImp(x, y) -> var_bool v x || var_bool v y
        | Imp(x, y) -> var_bool v x || var_bool v y
        | Neg x -> var_bool v x
				| _ -> false;;

	(* return an expression with the variable v replaced by assignment*)
    let rec var_lookup (v : expression) (e : expression) (asgmt : expression) 
        : expression = 
      match e with
        | True -> True
        | False -> False
				| Forall(_,x) -> var_lookup v x asgmt
				| Exists (_,x) -> var_lookup v x asgmt
        | Var i -> let Var j = v in if j = i then asgmt else e
        | And(x, y) -> And (var_lookup v x asgmt, var_lookup v y asgmt)
        | Or(x, y) -> Or (var_lookup v x asgmt, var_lookup v y asgmt)
        | BImp(x, y) -> BImp (var_lookup v x asgmt, var_lookup v y asgmt)
        | Imp(x, y) -> Imp (var_lookup v x asgmt, var_lookup v y asgmt)
        | Neg x -> Neg (var_lookup v x asgmt);;



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

	let rec print_bdd (a: bdd) = match a with 
		| Zero ->  " Zero "
		| One ->  " One "
		| Node(x,y,z) ->  " Node ( " ^ (string_of_int x) ^ "," ^ print_bdd y ^ "," ^ print_bdd z ^ " ) "
	
	let rec print_bdd2 (a: bdd) = match a with 
		| Zero -> print_string " Zero "
		| One ->  print_string " One "
		| Node(x,y,z) ->  print_string (" Node ( " ^ (string_of_int x) ^ ","); print_bdd2 y;
		print_string ","; print_bdd2 z; print_string " ) "
	
	let rec print_list = function 
	[] -> ()
	| e::l -> print_string e ; print_endline " " ; print_list l;;

	let rec print_exp_list = function 
	[] -> ()
	| e::l -> print_exp e ; print_endline " " ; print_exp_list l;;

	let rec print_exp_list_asis = function 
	[] -> ()
	| e::l -> print_exp_asis e ; print_endline " " ; print_exp_list_asis l;;
	
	let rec print_bdd_list = function 
	[] -> ()
	| e::l -> print_bdd2 e ; print_endline " " ; print_bdd_list l;;
	
	let createHT size = Hashtbl.create size;;
	
	let increment = let u = ref 1 in fun () -> incr u; !u;;

	let make (i :int) (low : bdd) (high : bdd) (h) (t) : bdd=
      if low == high then 
				begin 
					print_string "redundant test 1 - not adding node\n"; 
					print_string ("Low: "^print_bdd low^"\n");
					print_string ("High: "^print_bdd high^"\n");
					low 
				end
      else if Hashtbl.mem h (Node(i, low, high))
      then 
				begin
					print_string "redundant test 2 - not adding node\n";
					print_string ("Low: "^print_bdd low^"\n");
					print_string ("High: "^print_bdd high^"\n"); 
					Node(i, low, high)
				end
      else 
				begin
        print_string "Adding node\n";
				print_string ("Low: "^print_bdd low^"\n");
				print_string ("High: "^print_bdd high^"\n");
				let u = increment () in 
        let _ = Hashtbl.add h (Node(i, low, high)) u in
        let _ = Hashtbl.add t u (Node(i, low, high)) in
            Node(i, low, high)
				end
  
	let build (exp) (h) (t) : bdd = 
      let rec build' (e) (i : int) : bdd =
        match e with 
          | True -> 
              (* Add One to the hash table *)
              if Hashtbl.mem h (One) then One
              else
                let _ = Hashtbl.add h (One) (1) in 
                let _ = Hashtbl.add t 1 (One) in One
          | False -> 
              (* Add Zero to the hash table *)
              if Hashtbl.mem h (Zero) then Zero
              else
                let _ = Hashtbl.add h (Zero) (0) in 
                let _ = Hashtbl.add t 0 (Zero) in Zero
          | _ -> 
              (* Check if the variable i is in the expression, move on if not*)
              if var_bool (Var (string_of_int i)) e then
                (* recursively call build on low and high branches after var i
                 * is replaced with True for high branch and False for low
                 * branch *)
                let low = 
                  build' (eval (var_lookup (Var (string_of_int i)) e False)) (i+1) in
                let high = build' (eval (var_lookup (Var (string_of_int i)) e True)) (i+1) in
                  make (i) (low) (high) (h) (t)
              else build' (eval e) (i + 1) (* make creates the node *)
      in build' exp 1
    	
		
(*end;;*)