(** Evaluation of expressions, given as big step semantics. *)
  
  open Syntax
  
  (** [eval e] evaluates the expression [e] to an integer. It raises an
      expressions if division by zero occurs. *)
 

let rec eval fm =
  match fm with
    False -> false
  | True -> true
	| Var(p) -> true
  | Neg(p) -> not(eval p)
  | And(p,q) -> (eval p) & (eval q)
  | Or(p,q) -> (eval p) or (eval q)
  | Imp(p,q) -> not(eval p) or (eval q)
  | BImp(p,q) -> (eval p) = (eval q)
	| Forall(p,q) -> (eval q)
	| Exists(p,q) -> (eval q);;

(*let rec eval fm = print_string fm;;*)