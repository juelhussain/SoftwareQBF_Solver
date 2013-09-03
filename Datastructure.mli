module type DATASTRUCTURE = 
	sig 
		val getExpression: unit -> expression
	end;;
	
module Test = 
	struct
		type expression =
      | False
      | True
      | Var of int
      | And of expression * expression (* && *)
      | Or of expression * expression (* || *)
      | Imp of expression * expression (* ->, implication *)
      | BImp of expression * expression (* <->, bi-implication *)
      | Neg of expression (* -, negation *)
		
		let getExpression = True
	end;;