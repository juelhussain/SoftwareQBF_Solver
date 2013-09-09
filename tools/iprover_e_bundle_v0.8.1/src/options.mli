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


(*-----------------Option Types---------------------------*)

type out_options_type = Out_All_Opt | Out_Control_Opt | Out_No_Opt

type ground_splitting_type = Split_Input |Split_Full | Split_Off 

type schedule_type = Schedule_none |Schedule_default | Schedule_verification_epr


(*-----Lit Params----------*)

type lit_cmp_type = 
  | Lit_Sign    of bool 
  | Lit_Ground  of bool
  | Lit_Num_of_Var  of bool
  | Lit_Num_of_Symb of bool
  | Lit_Split       of bool 
  | Lit_has_conj_symb of bool 
  | Lit_has_non_prolific_conj_symb of bool  

(*----Clause Param---------*)
type cl_cmp_type = 
  |Cl_Age         of bool
  |Cl_Num_of_Var  of bool   
  |Cl_Num_of_Symb of bool   
  |Cl_Num_of_Lits of bool   
  |Cl_Ground      of bool
  |Cl_Conj_Dist   of bool
  |Cl_Has_Conj_Symb of bool
  |Cl_Has_Non_Prolific_Conj_Symb of bool
  |Cl_Max_Atom_Input_Occur of bool
  |Cl_Horn         of bool
  |Cl_EPR          of bool
  |Cl_Has_Eq_Lit   of bool


(*---Inst Lit Sel----*)

type inst_lit_sel_type    = lit_cmp_type list 
type pass_queue_type      = cl_cmp_type  list  

type inst_sel_renew_type = Inst_SR_Solver | Inst_SR_Model 


(*---------------------sat_out_model option types-----------------------------------*)

type sat_out_model_type = Model_Small | Model_Pos | Model_Neg | Model_Implied | Model_Debug |Model_None


(*--------------------Resolution Option Types--------------*)

(*----Subsumption----*) 
type res_subs_type = Subs_Full | Subs_Subset | Subs_By_Length of int

(*---Selection Fun----*)
type res_lit_sel_type = 
    Res_Adaptive | Res_KBO_Max | Res_Neg_Sel_Max | Res_Neg_Sel_Min

type res_to_prop_solver_type = 
    Res_to_Solver_Active | Res_to_Solver_Passive | Res_to_Solver_None


(*-----All options-----*)

(* Warning: functional options such as inst_lit_sel and inst_pass_queue1 *)
(* declare only types! if the options are changed, *)
(* one needs to change corresponding functions separately *)

type options = {
    mutable out_options           : out_options_type;

(*----Input-------*)
    mutable problem_path          : string; 
    mutable include_path          : string; 
    mutable problem_files         : string list;    
    mutable clausifier            : string;
    mutable clausifier_options    : string;
    mutable stdin                 : bool;

(*----General--------*)
    mutable fof                   : bool;    
    mutable time_out_real         : float;
    mutable time_out_virtual      : float;
    mutable schedule              : schedule_type;
    mutable ground_splitting      : ground_splitting_type;
    mutable non_eq_to_eq          : bool;
    mutable prep_prop_sim         : bool;
    mutable symbol_type_check     : bool;

(*---Large Theories---------------*)
    mutable large_theory_mode     : bool;
    mutable prolific_symb_bound   : int;
(*---threshold when the theory is considered to be large---*)
    mutable lt_threshold          : int;

(*----Sat Mode-----------*)
    mutable sat_mode              : bool; 
    mutable sat_gr_def            : bool;
    mutable sat_finite_models     : bool;
    mutable sat_out_model         : sat_out_model_type;

(*----Instantiation------*)
    mutable instantiation_flag                : bool;
    mutable inst_lit_sel                      : inst_lit_sel_type;  
    mutable inst_solver_per_active            : int;
    mutable inst_solver_per_clauses           : int;
    mutable inst_pass_queue1                  : pass_queue_type; 
    mutable inst_pass_queue2                  : pass_queue_type;
    mutable inst_pass_queue1_mult             : int;
    mutable inst_pass_queue2_mult             : int;
    mutable inst_dismatching                  : bool;
    mutable inst_eager_unprocessed_to_passive : bool;
    mutable inst_prop_sim_given               : bool;
    mutable inst_prop_sim_new                 : bool;
    mutable inst_learning_loop_flag           : bool;
    mutable inst_learning_start               : int;
    mutable inst_learning_factor              : int;
    mutable inst_start_prop_sim_after_learn   : int;
    mutable inst_sel_renew                    : inst_sel_renew_type;
    mutable inst_lit_activity_flag            : bool;

(*----Resolution---------*)
    mutable resolution_flag               : bool;
    mutable res_lit_sel                   : res_lit_sel_type;
    mutable res_to_prop_solver            : res_to_prop_solver_type;      
    mutable res_prop_simpl_new            : bool;
    mutable res_prop_simpl_given          : bool;

    mutable res_passive_queue_flag        : bool; 
    mutable res_pass_queue1               : pass_queue_type; 
    mutable res_pass_queue2               : pass_queue_type;
    mutable res_pass_queue1_mult          : int;
    mutable res_pass_queue2_mult          : int;
 
    mutable res_forward_subs              : res_subs_type; 
    mutable res_backward_subs             : res_subs_type; 
    mutable res_forward_subs_resolution   : bool;
    mutable res_backward_subs_resolution  : bool;
    mutable res_orphan_elimination        : bool;
    mutable res_time_limit                : float;
    mutable res_out_proof                 : bool;

(*----Combination--------*)
    mutable comb_res_mult                 : int;
    mutable comb_inst_mult                : int; 
  }

type named_options = {options_name : string; options : options}

val current_options : options ref

val input_options : options
val input_named_options : named_options 


(* if there is no conjectures then we can to remove corresponding comparisons*)

val strip_conj_named_opt : named_options -> named_options


(*--------Creates a reasonable option to deal with many axioms such as SUMO-----*)
(*-------based on a given option-------------------*)
val named_opt_to_many_axioms_named_opt1 : named_options -> named_options
val named_opt_to_many_axioms_named_opt2 : named_options -> named_options
val named_opt_to_many_axioms_named_opt3 : named_options -> named_options


val named_option_1   : unit -> named_options
val named_option_1_1 : unit -> named_options
val named_option_1_2 : unit -> named_options
val named_option_2   : unit -> named_options
val named_option_2_1 : unit -> named_options
val named_option_3   : unit -> named_options
val named_option_3_1 : unit -> named_options
val named_option_4   : unit -> named_options
val named_option_4_1 : unit -> named_options

val named_option_finite_models : unit -> named_options
val named_opt_sat_mode_off : named_options -> named_options
val named_opt_sat_mode_on  : named_options -> named_options

val named_option_epr_non_horn : unit -> named_options
val named_option_epr_horn     : unit -> named_options

val named_option_verification_epr : unit -> named_options

val res_lit_sel_type_to_str : res_lit_sel_type -> string
val get_problem_files : unit -> string list

val options_to_str : options -> string

(* inferred options: *)

val prop_solver_is_on : unit -> bool
