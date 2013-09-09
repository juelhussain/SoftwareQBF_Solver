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

(* non-perfect discr tree to be used as a filter for unif*)
open Lib
type term   = Term.term
type symbol = Symbol.symbol
type var    = Var.var
type sym_or_var = Sym of symbol | Var 


module type Param = 
  sig
    val num_of_symb : int
  end

module type DiscrTree =
  sig   
    type 'a index  

    val create        : unit -> 'a index
    val mem           : term -> 'a index -> bool 
    val find          : term -> 'a index -> 'a ref_elem
    val add_term_path : term -> ('a index) ref -> 'a ref_elem 
    val remove_term_path : term -> ('a index) ref -> unit
    val remove_term_path_ret : term -> ('a index) ref -> 'a ref_elem
    val unif_candidates : (('a list) index) -> term -> 'a list 
  end

module Make (P:Param) : DiscrTree
