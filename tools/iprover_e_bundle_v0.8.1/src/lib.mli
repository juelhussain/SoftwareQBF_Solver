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


val debug : bool

(* gets path to the iprover executable if defined by /proc/self/exe *)
(* else raises Not_found *)

val program_path : string
 
type 'a param = Def of 'a | Undef 

(* elements and ref to elem of indexies and all others*)

  type 'a elem = Elem of 'a | Empty_Elem
  type 'a ref_elem = ('a elem) ref

val get_some : 'a option -> 'a

exception Not_a_pair
val get_pair_from_list  : 'a list -> 'a * 'a

val clear_memory : unit -> unit 

val string_of_char : char -> string

(* fun is a function unit -> unit, get_time_fun returns time taken by fun  *)
(* truncated by tranc digits after . *)
val get_time_fun : int -> (unit->unit)-> float

(* truncates float to n digits after . *)
val truncate_n : int -> float -> float 

(* outcome of compare fun.*)
val cequal   : int
val cgreater : int
val cless    : int

(* *)
val param_str_ref : string ref 

val pref_str      : string

(* dash_str str:  ------- str ---------*)
val dash_str      : string -> string 

val add_param_str : string -> unit
val add_param_str_front : string -> unit
val param_str_new_line : unit -> unit

val compose_sign  : bool -> ('a -> 'b -> int) -> ('a -> 'b -> int)
(* hash sum where the first arg is rest and second is next number*)
val hash_sum : int -> int ->int 

exception Termination_Signal

(*----------------Processes-----------------*)
(* add_child_process pid *)
val add_child_process           : int -> unit 

(* add_child_process_channels (in_channel,out_channel,error_channel) *)
val add_child_process_channels  : 
    (in_channel * out_channel * in_channel) -> unit


(* removes from the list without killing *)
val remove_top_child_process_channels : unit -> unit 

val kill_all_child_processes : unit -> unit

(*----------------End Processes-----------------*)

(* composes functions *)

val compose_12   : ('a->'b)->('c->'d ->'a) -> 'c->'d -> 'b


(* used for localization of vars, binding can be 
   applied for vars, terms, clauses *)
type 'a bind = int * 'a

val  propagate_binding_to_list :  ('a list) bind -> ('a bind) list

(* lexicographic comparison of pairs*)
val pair_compare_lex : ('a -> 'a -> int)-> ('b -> 'b -> int) -> 'a*'b ->'a*'b -> int

(* bool operations *)
val bool_plus : bool -> bool -> bool

(*lists*)
val list_skip : 'a -> 'a list -> 'a list

(* explicitly maps from left to right, 
   since order can matter when use imperative features *)
val list_map_left : ('a -> 'b) -> 'a list -> 'b list

val list_to_string : ('a -> string) -> 'a list -> string -> string

val list_of_str_to_str : (string list) -> string -> string

val list_findf : ('a -> 'b option) -> 'a list -> 'b option

val out_str : string -> unit
(* out if debug is on *)
(*val out_str_debug : string -> unit*)

val list_compare_lex : ('a -> 'a -> int) -> 'a list -> 'a list ->int
val lex_combination  : ('a -> 'a -> int) list -> 'a -> 'a -> int

(* in list_is_max_elem and list_get_max_elements
   we assume that compare as follows: 
   returns cequal if t greater or equal to s and 
   returns cequal+1 if t is strictly greater
   returns cequal-1 if it is not the case
  Note: it is assumed that 
   if t (gr or eq) s and s (gr or eq) t then t==s
*)    

val list_is_max_elem_v :   ('a -> 'a -> int) -> 'a -> 'a list -> bool

val list_get_max_elements_v : ('a -> 'a -> int) -> 'a list -> 'a list

(* for usual orderings *)
val list_is_max_elem :   ('a -> 'a -> int) -> 'a -> 'a list -> bool

(* finds max element in the list if the list is empty raises Not_found*)
val list_find_max_element : ('a -> 'a -> int) -> 'a list -> 'a

val list_find_max_element_p : ('a -> bool) -> ('a -> 'a -> int) -> 'a list -> 'a

(* removes duplicates  based on the fact 
  that literals are preordered i.e. the same are in sequence*)

val list_remove_duplicates : ('a list) -> ('a list)

val list_find2 : ('a -> 'b -> bool) -> ('a list) -> ('b list) -> ('a *'b) 

val list_return_g_if_f2 : 
    ('a -> 'b -> bool) -> ('a -> 'b -> 'c) -> ('a list) -> ('b list) -> 'c

(* finds first el. a' b' not equal by compare_el, 
  which suppose to return ctrue if equal,
  and returns compare_el 'a 'b 
*)

val list_find_not_equal :  
    ('a -> 'b -> int) -> ('a list) -> ('b list) -> int
	
val list_find_not_identical :
    ('a list) -> ('a list) -> 'a * 'a

(* association lists *)

type ('a, 'b) ass_list = ('a*'b) list

(* appends ass lists: if list1 and list2 have
 elem with (k,v1)  and (k,v2) resp. then new list will have (k,(f !v1 !v2))
 otherwise  appends (k1,v1) and (k2,v2)*)

val append_ass_list : 
    ('b -> 'b -> 'b) -> ('a, 'b) ass_list -> ('a, 'b) ass_list -> ('a, 'b) ass_list

type 'a num_ass_list =  ('a,int) ass_list

(*----------- Output Buffers/Channels ----------------------*)

(* string stream can be e.g. a buffer or a channel *)

type 'a string_stream = 
    {
     stream : 'a;
     stream_add_char : char   -> unit;
     stream_add_str  : string -> unit;
   }

val stdout_stream : out_channel string_stream
   
(*  "list_to_stream s to_str_el l separator_str" itreates "to_str_el s el" *)
(* over all elements in l and adds sepearator str in between elem. *)

val list_to_stream : ('a string_stream) ->  
                     (('a string_stream) -> 'b -> unit) ->
		     ('b list) -> 
		      string -> 
		      unit

(* "let to_string = to_string_fun_from_to_stream_fun 30 to_stream" *)
(*    creates to_string function from to_stream function with      *)
(*    initial buffer size 30                                       *)


val to_string_fun_from_to_stream_fun :
           int -> (Buffer.t string_stream -> 'a -> 'b) -> 'a -> string
(*
val to_string_fun_from_to_stream_fun :
    int->
    ('a string_stream -> 'b -> unit) ->
    ('b -> string) 
*)

val create_buffer_stream : int -> (Buffer.t string_stream) 

val to_string_buffer_stream :  (Buffer.t string_stream) -> string




val param_to_string : ('a -> string) -> 'a param -> string

val param_to_stream : 
    ((('a string_stream) -> 'b -> unit )-> 
      ('a string_stream)  -> 'b param -> unit)

(*---------strings-----------*)

(*string filled with n spaces *)
val space_str        :  int -> string 

val to_stream_space : 'a string_stream -> int -> unit

(* add spaces to str to reach distance *)
(*if the distance is less than or equal to str then just one space is added*)
(*(used for formatting output) *)
val space_padding_str :  int -> string -> string


(*--------Named modules----------------------*)

module type NameM = 
  sig
    val name : string
  end



(*--------------Global Time Limits-------------------*)
(* time limit in seconds *)
(* time_limit can be reassigned, there are number of points where it is checked*)


exception Timeout

(*---------Discount time limits can be checked in all related modules-------*)
(* After Timeout using discount can be incomplete (bit still sound) *)

val assign_discount_time_limit :float -> unit 
val assign_discount_start_time : unit -> unit
val check_disc_time_limit : unit -> unit
