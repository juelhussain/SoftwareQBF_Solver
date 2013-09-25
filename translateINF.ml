(*module TranslateINF = 
	struct*)
		open Syntax;;
		
		
		(* This function will take an OBDD and convert to the propositional *)
		(* formula representation. This is for visualisation. for the data representation *)
		(* conversion use translate_to_cnf_F*)
		let translate_to_cnf (inputBDD) =	
		let rec nextLevel(var) = 
				match var with 
				| One -> " True "
				| Zero -> " False "
				| Node(x,y,z) -> let obddformula = 
					"(~ "^(string_of_int x)^" or "
					^(nextLevel(z))^") AND ("
					^(string_of_int x)^" or "
					^(nextLevel(y))^")" in obddformula
			in nextLevel(inputBDD) 
		
		(* This function will convert an OBDD to an expression represnetation of*)
		(* propositional logic. This can be further reduced to CNF using the to_cnf*)
		(* function. *)
		
		let translate_to_cnf_F (inputBDD) =	
		let rec nextLevel(var) = 
				match var with 
				| One -> True
				| Zero -> False
				| Node(x,y,z) -> 
					let obddformula = 
					And(
						Or(Neg(Var (string_of_int x)),nextLevel(z))
						, 
					  Or(Var (string_of_int x),nextLevel(y))
					)
					in obddformula
			in nextLevel(inputBDD) 
			
		(* Takes an expression and converts to CNF representation. *)
		
		
		(*let rec to_cnf_with_contra formula =
			match formula with 
			| BImp(p,q) -> to_cnf(And(Imp(to_cnf p, to_cnf q),Imp( to_cnf q,to_cnf p)))
			| Imp(p,q) -> to_cnf(Or(Neg(to_cnf p), to_cnf q))
			| Neg(Or(p,q)) -> to_cnf(And(Neg(to_cnf p),Neg(to_cnf q)))
			| Neg(And(p,q)) -> to_cnf(Or(Neg(to_cnf p),Neg(to_cnf q)))
			| Neg(Neg(p)) -> to_cnf p
			| And(p,True) -> to_cnf p | And(True,p) -> to_cnf p
			| And(p,False) -> False | And(False,p) -> False
			| And(p,Neg(q)) -> if (p=q) 
					then (to_cnf p) 
					else And(to_cnf p, to_cnf (Neg(q)))
		  | And(Neg(p),q) -> if (p=q) then (to_cnf q) else And(to_cnf (Neg(p)), to_cnf q)
			| Or(p,True) -> True | Or(True,p) -> True
			| Or(p,False) -> to_cnf p | Or(False,p) -> to_cnf p 
			| Neg(True) -> False
			| Neg(False) -> True
			| Or(And(p,q),r) -> to_cnf(And(Or(to_cnf p, to_cnf r),Or(to_cnf q, to_cnf r)))
			| Or(r,And(p,q)) -> to_cnf(And(Or(to_cnf p, to_cnf r),Or(to_cnf q, to_cnf r)))
			| And(p,q) -> And(to_cnf p, to_cnf q)
			| Or(p,q) -> Or(to_cnf p, to_cnf q)
			| Neg(p) -> Neg(to_cnf p)
			| _ -> formula*)
		
		let rec to_cnf formula =
			match formula with 
			| BImp(p,q) -> to_cnf(And(Imp(to_cnf p, to_cnf q),Imp( to_cnf q,to_cnf p)))
			| Imp(p,q) -> to_cnf(Or(Neg(to_cnf p), to_cnf q))
			| Neg(Or(p,q)) -> to_cnf(And(Neg(to_cnf p),Neg(to_cnf q)))
			| Neg(And(p,q)) -> to_cnf(Or(Neg(to_cnf p),Neg(to_cnf q)))
			| Neg(Neg(p)) -> to_cnf p
			| And(p,True) -> to_cnf p | And(True,p) -> to_cnf p
			| And(p,False) -> False | And(False,p) -> False
			| Or(p,True) -> True | Or(True,p) -> True
			| Or(p,False) -> to_cnf p | Or(False,p) -> to_cnf p 
			| Neg(True) -> False
			| Neg(False) -> True
			| Or(And(p,q),r) -> to_cnf(And(Or(to_cnf p, to_cnf r),Or(to_cnf q, to_cnf r)))
			| Or(r,And(p,q)) -> to_cnf(And(Or(to_cnf p, to_cnf r),Or(to_cnf q, to_cnf r)))
			| And(p,q) -> And(to_cnf p, to_cnf q)
			| Or(p,q) -> Or(to_cnf p, to_cnf q)
			| Neg(p) -> Neg(to_cnf p)
			| _ -> formula
		
		let to_cnf2 formula = 
			let first = to_cnf (formula)
			in to_cnf(first)
	
		let to_cnf_list cnf_list = 
			let rec cnf2 new_cnf_list i= 
				if (i>=List.length cnf_list) then (List.rev new_cnf_list)
				else
				cnf2 ((to_cnf2 (List.nth cnf_list i))::new_cnf_list) (i+1)
			in cnf2 [] 0
		
		let translate_list_to_cnf (bddList) =
			let rec translate (cnf_list) i = 
				if (i>=(List.length bddList)) then (List.rev cnf_list)
				else
					begin
						let bdd_element = (List.nth bddList i) in
							translate (translate_to_cnf_F(bdd_element)::cnf_list) (i+1)
					end
			in translate [] 0
		
		let rec remove_contras fm = 
			match fm with
			| And(p,Neg(q)) -> if (p=q) 
					then (remove_contras p) 
					else And(p, remove_contras (Neg(q)))
		  | And(Neg(p),q) -> if (p=q) then (remove_contras q) else And(Neg(p), remove_contras q)
			| _ -> fm
			
		let rec to_cnf_with_contra formula =
			match formula with 
			| BImp(p,q) -> to_cnf_with_contra(And(Imp(to_cnf_with_contra p, to_cnf_with_contra q),Imp( to_cnf_with_contra q,to_cnf_with_contra p)))
			| Imp(p,q) -> to_cnf_with_contra(Or(Neg(to_cnf_with_contra p), to_cnf_with_contra q))
			| Neg(Or(p,q)) -> to_cnf_with_contra(And(Neg(to_cnf_with_contra p),Neg(to_cnf_with_contra q)))
			| Neg(And(p,q)) -> to_cnf_with_contra(Or(Neg(to_cnf_with_contra p),Neg(to_cnf_with_contra q)))
			| Neg(Neg(p)) -> to_cnf_with_contra p
			| And(p,True) -> to_cnf_with_contra p | And(True,p) -> to_cnf_with_contra p
			| And(p,False) -> False | And(False,p) -> False
			| And(p,Neg(q)) -> if (p=q) 
					then (to_cnf_with_contra p) 
					else remove_contras And(p,Neg(q))
		  | And(Neg(p),q) -> if (p=q) then (to_cnf_with_contra q) 
					else remove_contras And(Neg(p),q)
			| Or(p,True) -> True | Or(True,p) -> True
			| Or(p,False) -> to_cnf_with_contra p | Or(False,p) -> to_cnf_with_contra p 
			| Neg(True) -> False
			| Neg(False) -> True
			| Or(And(p,q),r) -> to_cnf_with_contra(And(Or(to_cnf_with_contra p, to_cnf_with_contra r),Or(to_cnf_with_contra q, to_cnf_with_contra r)))
			| Or(r,And(p,q)) -> to_cnf_with_contra(And(Or(to_cnf_with_contra p, to_cnf_with_contra r),Or(to_cnf_with_contra q, to_cnf_with_contra r)))
			| And(p,q) -> And(to_cnf_with_contra p, to_cnf_with_contra q)
			| Or(p,q) -> Or(to_cnf_with_contra p, to_cnf_with_contra q)
			| Neg(p) -> Neg(to_cnf_with_contra p)
			| _ -> formula


		(* This function converts back a variable assignment that was given using the *)
		(* hashtable index. *)
		let rec replace_vars v h = 
  		match v with
  		| False -> False
  		| True -> True
  		| Var(p) -> Var(string_of_int (Hashtbl.find h p))
  		| Neg(p) -> Neg(replace_vars p h)
      | And(p, q) -> And(replace_vars p h, replace_vars q h)
      | Or(p, q) -> Or(replace_vars p h, replace_vars q h)
      | Imp(p, q) -> Imp(replace_vars p h, replace_vars q h)
      | BImp(p, q) -> BImp(replace_vars p h, replace_vars q h)
    	| Forall(p, q) -> Forall(string_of_int (Hashtbl.find h p), replace_vars q h)
    	| Exists(p, q) -> Exists(string_of_int (Hashtbl.find h p), replace_vars q h)
  
		
	
(*end;;*)
		
		
		