(** Abstract syntax. *)
  
	(*type formula = False
                 | True
								 | Var of string 
                 | Not of formula
                 | And of formula * formula
                 | Or of formula * formula
                 | Imp of formula * formula
                 | Iff of formula * formula
                 | Forall of string * formula
                 | Exists of string * formula;;*)

	type expression =
      | False
      | True
      | Var of string
      | And of expression * expression (* && *)
      | Or of expression * expression (* || *)
      | Imp of expression * expression (* ->, implication *)
      | BImp of expression * expression (* <->, bi-implication *)
      | Neg of expression (* -, negation *)
			| Forall of string * expression
      | Exists of string * expression;;

	
  (** Arithmetical expressions. *)
  type expression2 =
    | Numeral of int (** non-negative integer constant *)
    | Plus of expression2 * expression2  (** Addition [e1 + e2] *)
    | Minus of expression2 * expression2 (** Difference [e1 - e2] *)
    | Times of expression2 * expression2 (** Product [e1 * e2] *)
    | Divide of expression2 * expression2 (** Quotient [e1 / e2] *)
    | Negate of expression2 (** Opposite value [-e] *)
  
  (** Conversion of expresions to strings. *)
  let string_of_expression e =
    let rec to_str n e =
      let (m, str) = match e with
          Numeral n       ->    (3, string_of_int n)
        | Negate e        ->    (2, "-" ^ (to_str 0 e))
        | Times (e1, e2)  ->    (1, (to_str 1 e1) ^ " * " ^ (to_str 2 e2))
        | Divide (e1, e2) ->    (1, (to_str 1 e1) ^ " / " ^ (to_str 2 e2))
        | Plus (e1, e2)   ->    (0, (to_str 0 e1) ^ " + " ^ (to_str 1 e2))
        | Minus (e1, e2)  ->    (0, (to_str 0 e1) ^ " - " ^ (to_str 1 e2))
      in
        if m < n then "(" ^ str ^ ")" else str
    in
     to_str (-1) e