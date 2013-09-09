(*----------------------------------------------------------------------(C)-*)
(* Copyright (C) 2006-2010 Konstantin Korovin and The University of Manchester. 
   This file is part of iProver - a theorem prover for first-order logic.

   iProver is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   iProver is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
   See the GNU General Public License for more details.
   You should have received a copy of the GNU General Public License
   along with iProver.  If not, see <http://www.gnu.org/licenses/>.         *)
(*----------------------------------------------------------------------[C]-*)

type symb   = Symbol.symbol
type term   = Term.term 
type clause = Clause.clause

(* one should run flat_signature before any other function*)
val flat_signature : unit -> unit


val flat_clause : clause -> clause
val flat_clause_list : clause list -> clause list


val add_domain_constant  : int -> term
val add_domain_constants : int -> int -> term list
val add_domain_pred      : int -> term

val dis_eq_axioms      : term -> term list -> clause list
val dis_eq_axioms_list : term list -> clause list

val domain_axioms            : term -> term list ->  clause list
val domain_axioms_triangular : term -> term list ->  clause list
val domain_axioms_unit       : term -> term list ->  clause list
