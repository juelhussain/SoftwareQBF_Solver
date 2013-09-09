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

open Lib

type symbol

type t=symbol

type fast_key 
type stype = 
  |Fun|Pred|Connective|Quantifier| Undef_stype

(* Split is pred indroduced in splitting and Flat in flattening*)
(* Domain constants and DefPred used in finite models *)

(* FunPred are Functions obtained by Non-Eq preds -> Eq translation *)
type sproperty = Theory|Split|Flat|FunPred| DomainConstant|DomainPred|DefPred

(* Special Symbols*)
val symb_and        : symbol
val symb_or         : symbol
val symb_impl       : symbol
val symb_forall     : symbol
val symb_exists     : symbol
val symb_neg        : symbol
val symb_true       : symbol
val symb_false      : symbol
val symb_equality   : symbol
val symb_plus       : symbol
val symb_minus      : symbol
val symb_unaryminus : symbol
val symb_iprover_eq : symbol
val symb_bot        : symbol
val symb_top        : symbol

(* list of all symbols above *)
val special_symbols : symbol list

exception Symbol_fast_key_undef
exception Arity_undef
exception Group_undef

(* use SymbolDB.get_num_of_sym_groups to get the actual number of groups*)
val max_num_of_sym_groups : int

val  create_from_str       : string -> stype -> symbol
val  create_from_str_arity : string -> stype -> int -> symbol 

(* fast key assigned when symbolDB is creating*)
val  assign_fast_key       : symbol -> int -> unit
(*val  assign_hash           : symbol -> int -> unit*)

val assign_group           : symbol -> int -> unit
val assign_is_input        : bool -> symbol  -> unit
val assign_is_skolem       : bool -> symbol  -> unit
val assign_property        : sproperty -> symbol -> unit

(* bool params *)
type symbol_bool_param 

val is_conj_symb               : symbol_bool_param
val large_ax_considered_gr        : symbol_bool_param
val large_ax_considered_non_gr    : symbol_bool_param

val is_constant                 : symbol -> bool
val get_name                    : symbol -> string

val set_bool_param : bool ->  symbol_bool_param -> symbol -> unit
val get_bool_param : symbol_bool_param -> symbol -> bool 
(* inherit_bool_param param from_s to_s *)
val inherit_bool_param     :  symbol_bool_param -> symbol -> symbol -> unit
(* inherit_bool_param_all  from_s to_s *)
val inherit_bool_param_all :  symbol -> symbol -> unit

val get_num_input_occur   : symbol -> int
val incr_num_input_occur  : symbol -> unit


val to_stream             : 'a string_stream -> symbol -> unit
val out                   : symbol -> unit
val prolog_to_stream      : 'a string_stream -> symbol -> unit

val to_string             : symbol -> string
val to_prolog             : symbol -> string

val to_stream_full        : 'a string_stream -> symbol -> unit
val to_string_full        : symbol -> string

val get_arity             : symbol -> int
val get_type              : symbol -> stype
val get_group             : symbol -> int
val is_input              : symbol -> bool

(* used for flattening transform where each fun symbol *)
(* is associated with a predicate *)
(* assign_flattening  s flat *)
val assign_flattening      : symbol -> symbol -> unit
val get_flattening         : symbol -> symbol
val is_flat                : symbol -> bool
val is_defpred             : symbol -> bool

(*can raise Undef*)
val  is_skolem             : symbol -> bool
val  get_property          : symbol -> sproperty param
   
(* compare = compare_fast_key and should not be used before 
   fast_key is assigned i.e. symbolDB build (the same to equal and hash); 
   before that use compare_key *)  


val compare               : symbol -> symbol -> int 
val equal                 : symbol -> symbol -> bool
val compare_key           : symbol -> symbol -> int
val compare_fast_key      : symbol -> symbol -> int

(* hash is random number, small hash is number below num of symbols in db *)
(*val get_hash              : symbol -> int*)
val hash_small            : symbol -> int
val get_fast_key          : symbol -> int
