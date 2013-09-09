module Datastructure = 
	struct
		type expression =
      | False
      | True
      | Var of string
      | And of expression * expression (* && *)
      | Or of expression * expression (* || *)
      | Imp of expression * expression (* ->, implication *)
      | BImp of expression * expression (* <->, bi-implication *)
      | Neg of expression (* -, negation *);;
		
		 (* abstract data type for Boolean operations, used in apply *)
    type operation = 
      | OpAnd
      | OpOr
      | OpImp
      | OpBimp;;
    
		type bdd = Zero | One | Node of int * bdd * bdd;;
		
		
	
	end;;