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



val fill_tables                : clause list -> unit

val get_symb_list              : clause list -> symb list

val get_axioms_next_keys_all   : symb list -> (clause list)* (symb list)

(*
val increase_prolific_bound_by : int -> unit 
*)
val get_all_non_considered     : clause list -> clause list
