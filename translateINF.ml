module TranslateINF = 
	struct
		open Syntax;;
		
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
		
		let rec to_cnf (formula)= 
			match formula with 
			| True -> True
			| False -> False
			| Var p -> Var p
			| BImp(p,q) -> And(Imp(to_cnf p, to_cnf q),Imp( to_cnf q,to_cnf p))
			| Imp(p,q) -> Or(Neg(to_cnf p), to_cnf q)
			| Neg(Or(p,q)) -> And(Neg(to_cnf p),Neg(to_cnf q))
			| Neg(And(p,q)) -> Or(Neg(to_cnf p),Neg(to_cnf q))
			| Neg(Neg(p)) -> to_cnf p
			| And(p,True) -> to_cnf p | And(True,p) -> to_cnf p
			| And(p,False) -> False | And(False,p) -> False
			| Or(p,True) -> True | Or(True,p) -> True
			| Or(p,False) -> to_cnf p | Or(False,p) -> to_cnf p 
			| Neg(True) -> False
			| Neg(False) -> True
			| Or(And(p,q),r) -> And(Or(to_cnf p, to_cnf r),Or(to_cnf q, to_cnf r))
			| Or(r,And(p,q)) -> And(Or(to_cnf p, to_cnf r),Or(to_cnf q, to_cnf r))
			| And(p,q) -> And(to_cnf p, to_cnf q)
			| Or(p,q) -> Or(to_cnf p, to_cnf q)
			| Neg(p) -> Neg(to_cnf p)
			| _ -> formula

		let rec to_cnf (formula)= 
			match formula with 
			| True -> True
			| False -> False
			| Var p -> Var p
			| BImp(p,q) -> to_cnf(And(Imp(p, q),Imp(q,p)))
			| Imp(p,q) -> to_cnf(Or(Neg(p),q))
			| Neg(Or(p,q)) -> to_cnf(And(Neg(p),Neg(q)))
			| Neg(And(p,q)) -> to_cnf(Or(Neg(p),Neg(q)))
			| Neg(Neg(p)) -> to_cnf p
			| And(p,True) -> to_cnf p | And(True,p) -> to_cnf p
			| And(p,False) -> False | And(False,p) -> False
			| Or(p,True) -> True | Or(True,p) -> True
			| Or(p,False) -> to_cnf p | Or(False,p) -> to_cnf p 
			| Neg(True) -> False
			| Neg(False) -> True
			| Or(And(p,q),r) -> to_cnf(And(Or( p, r),Or(q, r)))
			| Or(r,And(p,q)) -> to_cnf(And(Or( p, r),Or( q, r)))
			| And(p,q) -> And(to_cnf p, to_cnf q)
			| Or(p,q) -> Or(to_cnf p, to_cnf q)
			| Neg(p) -> Neg(to_cnf p)
			| _ -> formula
		
		let rec to_cnf (formula)= 
			match formula with 
			| True -> True
			| False -> False
			| Var p -> Var p
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
  
  
end;;
		
		
		