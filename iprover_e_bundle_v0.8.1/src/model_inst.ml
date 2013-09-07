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
open Options

type lit    = Term.term
type term   = Term.term
type clause = Clause.clause 
type var    = Var.var



type all_clauses = ClauseAssignDB.clauseDB

let term_db_ref    = Parsed_input_to_db.term_db_ref

type raw_model = all_clauses 


(* model node consists of a normalised literal, list of clauses  *)
(* where this literal is selected normalised w.r.t. the literal  *)
(*   *)
(*
type model_node = 
    {
     model_lit : lit; 
(* *)
     mutable clause_set : 
       mutable constraint_set :  
   }
*)

(*
let build_model active_clauses = active_clauses
*)



(*----------------------------------------------------*)
let raw_model_to_stream s model = 
  let f clause = 
    if not (Clause.get_bool_param Clause.in_active clause)
    then ()
    else
      begin
	s.stream_add_str "%---------------\n";
	Clause.to_stream s clause;
	s.stream_add_char ' ';
	Term.to_stream s (Clause.get_inst_sel_lit clause);
	s.stream_add_char '\n';
	(try 	
	  Dismatching.to_stream_constr_set s (Clause.get_dismatching clause);
	  s.stream_add_char '\n';
	with 
	  Clause.Dismatching_undef ->
	    s.stream_add_str "[]\n");	
      end
  in
  ClauseAssignDB.iter f model;
  s.stream_add_str "\n%---------------End Model--------------\n\n"


let out_raw_model model = 
  raw_model_to_stream stdout_stream model


(*--------------------------------------------------------------------*)
(* Model representation:                                              *)
(* Several models can be extracted: 1) Positive 2) Negative 3) Mixed  *)
(* leterals a defined in the fixed model of the term algebra          *)
(*--------------------------------------------------------------------*)
(*
  \forall x_1,...,x_n
   (~) L(x_1,..,x_n) <->  
(* we will  call it definition of a literal lit_def *)
     [
  
         \exists \bar{y}
          [
            (x_i_1 = t_1(\bar{y},\bar{x}) &..& x_i_m = t_m(\bar{y},\bar{x})  
        (* this corresponds to flattening of L(x,t,..) *)
        (* let's call it subst_constr this also corresponds to the selected literal in a clause *)
        (* we will pair list of clauses with this literal selected  with the set of dismatching constraints below *)
  
         (* variables from \bar{y} can contain  some x_j below *)
           &

            (* this correspods to dismathcing  constraints collected from  *)
            (* all active clauses where L(x,t,..) is selected              *)

             \forall \bar{z} ((x_j_1 \not = s_1(\bar{z}) \/...\/ x_j_n \not = s_n(\bar{z})))&
              ..............
             \forall \bar{z} ((x_l_1 \not = g_1(\bar{z}) \/...\/ x_j_n \not = g_v(\bar{z})))
            ]
      \/
      ......      
      \/   
      ......
   ]  

(* both subst_constr and dismathcing  constraints are represented as flat_subst, ordered and normalised *)
*)

(* there are three basic types of models positive, negative and implied *)
(* positive -- using positive definitions (\\forall x_1,..,x_n  (P(x_1,..,x_n) <=> (\\phi(x_1,..,x_n))))  *)
(* negative -- using negative definitions (\\forall x_1,..,x_n  (~P(x_1,..,x_n) <=> (\\phi(x_1,..,x_n)))) *)
(* small -- choosing between positive and negative definitions according to the size                      *)
(* implied -- definitions of the form (\\forall x_1,..,x_n  ((~)P(x_1,..,x_n) <= (\\phi(x_1,..,x_n))))    *)
(*            if for some arguments a value of a predicate is not implied by these definitions            *)
(*            then we can complete the model by choosing an arbitrary value of this predicate on these arguments  *)
(* def_type is used to choose between <= and <=> and also when lit is undefined and <=> we need to add <=> $false *)
(* and when <= we can ommit the definition *)

type def_type =  Def_Equiv| Def_Implied 

(* positive:  *)

type flat_subst = Subst.flat_subst



(*--------------to_stream------------------*)
let model_pref_str = 
  "%"^pref_str
      
let var_list_to_stream s v_list = 
  List.iter (fun v -> 
    Var.to_stream s v;
    s.stream_add_char ' '
	    ) v_list


let var_term_to_stream s (v,t) =  
  Term.to_stream s t;
  s.stream_add_char '/';
  Var.to_stream s v

let to_stream_plain_fs s constr =  
  s.stream_add_char '[';
  Lib.list_to_stream s var_term_to_stream constr ",";
  s.stream_add_char ']'

(* when we normalise clauses we have lit lists where the first literal is selected*)
let to_stream_plain_lit_list_first_selected s ll = 
  s.stream_add_char '{';
 ( match ll with 
 |h::tl -> 
      s.stream_add_char '|';
     Term.to_stream s h; 
     s.stream_add_char '|';
     (if (not (tl = []))
     then 
       (s.stream_add_char ';';)
     else ()
     );
     Lib.list_to_stream s Term.to_stream tl ";";
  |[] -> failwith "model_inst: to_stream_plain_lit_list_first_selected at least one literal should be present"
  );
  s.stream_add_char '}'


(*------------------------------*)

let compare_vt (v1,t1) (v2,t2) = 
  let v_cp = Var.compare v1 v2 in
  if v_cp = 0 
  then 
      Term.compare t1 t2 
  else
    v_cp

(*[] is the smallest element, which is used to output [] first in maps and sets *)
let rec compare_flat_subst s1 s2 = 
  match (s1, s2) with 
  |(h1::tl1,h2::tl2) -> 
      let h_cp = compare_vt h1 h2 in 
      if h_cp = 0 
      then 
	compare_flat_subst tl1 tl2
      else
	h_cp
  |(h1::tl,[]) -> 1
  |([],h1::tl) -> -1 
  |([],[])     -> 0 


type flat_subst_vars =  
    {
     flat_subst : flat_subst;
     (* flat_subst_vars is ordered list of vars \bar{y} in t_1(\bar{y})/x_0 ... t_n(\bar{y})/x_n *)
     (* used when we output the constraints  *)
     flat_subst_vars    : var list
   }

(*we need to order flat_subst before adding to a set!*)
module FSVKey = 
  struct
    type t = flat_subst_vars
    let compare fsv1 fsv2 = compare_flat_subst fsv1.flat_subst fsv2.flat_subst
  end

module FSVSet = Set.Make (FSVKey)

module FSVMap = Map.Make (FSVKey)

module VarMap = Map.Make (Var)



type dism_constr_set = 
  |DCSet of FSVSet.t
(* Subsumed means that all dismatching constraints are subsumed     *)
(* by dismatching constr which is either undefined of is a renaming *)
(* (over all vars in the clause)           *)
(* so L(X)| (x=t & Subsumed ...) means \forall X (L(X) <=> (x=t...) *)
  |DCSSubsumed

type var_table = (var VarMap.t)

type var_table_ref = var_table ref

(*we pair old clause with the normalised lit list form this clause and constraint list*)
type norm_clause = 
    (clause * ((lit list) * flat_subst list))
(* literal definition body is a map from subst_constraints into sets of dismatching constrints *)
(* paired with a list of clauses where the L\subst_constraint is selected  *)
(* dismatching constraints are normalised and only varibales in L are left *)
(*  clauses are also normalised such that L is first, vars renamed, including in dismatiching constr.*)

let to_stream_plain_norm_clause s (_clause, ((lit_list), flat_subst_list)) = 
  s.stream_add_str "% ";
  to_stream_plain_lit_list_first_selected s lit_list;
  s.stream_add_str "\n% ";  
  Lib.list_to_stream s to_stream_plain_fs flat_subst_list "\n% ";
  s.stream_add_char '\n'

let to_stream_plain_norm_clause_list s norm_clause_list = 
  s.stream_add_str model_pref_str;  
  s.stream_add_char '\n';  
  Lib.list_to_stream s to_stream_plain_norm_clause norm_clause_list (model_pref_str^"\n");
  s.stream_add_char '\n'


  
type lit_def_body = 
    (((dism_constr_set ref) * ((norm_clause list) ref)) FSVMap.t)


type lit_def = 
    {

(* this is a flattened normalised literal of the form (~)P(x_0,..,x_n)                       *)
     model_lit          : lit; 

(* ordered list of vars in the literal                                                       *)
     model_lit_vars     : var list; 

(* a) if lit_def_body is FSVMap.empty then this literal was not defined and this corresponds to          *)
(*    \forall X ~L(X)    or \forall X (L(X) <=> $false) when output  sign(L(X)) type definition,         *)
(*    if output "implied" definition then we do not need to output anyhting here (it's undefined)        *)
(*    ( compl(sign L(X)) still can have "implied" definition)                                            *)
(* b) if [] is a key in  lit_def_body, this corresponds to the empty subst_constr, there are still       *)
(*    can be dismatching constraints corresponding to [], *)
(*   But if the dism constr. set is empty/Subsumed then   *)
(*   we have \forall X (L(X) <->  $true)   (all other instances of L(X) are subsumed by this)    *)
(*   in this case we set always_true to true but still fill lit_def_body (for extra information/normalise clauses) *)
(*   the number of constraints is 0 in this case                                                                   *)

     mutable model_lit_def_body     : lit_def_body;

(*----- below are filled when we constructed the model (could be filled during but too messy)---------*)    
(* if model_lit_undef = true then we in the a) situation *)

     mutable model_lit_undef                  : bool;
     mutable model_lit_always_true            : bool;
     mutable model_lit_num_of_subst_constr    : int;
     mutable model_lit_num_of_dism_constr     : int; 
   }

type model_node = 
    {
    (* positive and negative definitions of the same predicate *)
     pos_lit_def : lit_def; 
     neg_lit_def : lit_def; 
   } 

(* normal model is a map from predicates to model nodes definig these predicates *)
module NModel =  Map.Make(Symbol)

type norm_model = (model_node NModel.t)


(* for a list of vars h::tl  returns var  next(h) *)
(* for [] returns first var                       *)

let next_top_var vlist = 
  match vlist with 	
  |h::_ -> Var.get_next_var h
  |[]   -> Var.get_first_var () 
	
   

(*------------------------------------*)

(* var_list contains the list of new vars, max_var first *)
(* note new terms are not in term_db! *)
(* new_var_list_ref contains newly introduced vars, *)
(* normally is empty at the beginning *)

let rec normalise_term_var' var_table_ref max_var_ref var_list_ref (*new_var_list_ref*) term =
  match term with
  | Term.Fun(sym,args,_) ->
      let new_args = 
	Term.arg_map_left (normalise_term_var' var_table_ref max_var_ref var_list_ref) args in
      Term.create_fun_term_args sym new_args 
  | Term.Var(v,_) -> 
      try
	let new_v = VarMap.find v !var_table_ref in 
	Term.create_var_term new_v
      with 
	Not_found -> 
	  var_table_ref := VarMap.add v !max_var_ref !var_table_ref;
	  var_list_ref  := (!max_var_ref)::(!var_list_ref);	  
	  let new_term = Term.create_var_term !max_var_ref in	  
	  max_var_ref := Var.get_next_var !max_var_ref;
	  new_term
	  
	    


(* takes an atom A(t0,..,tn) and makes  A(x_0,x_1,..,x_n)                     *)
(* var_table is a renaming from old varibles into new                         *)
(* (a map from old var to new vars)                                           *)
(* flat_subst will contain [(x_i_0,t'i_0));..;(x_i_k, t'_i_k)]                *)
(* where t'i_j  is normalised ti_j according to the renaming                  *)
(* and ti_j is either not a variable or a variable which has occurred before  *)
(*  t_'_i_j can contain both x-vars (vars from x_0,..,x_n)                    *)
(*  and y-vars (not x-vars), y-vars are existentially quanitifiered later     *)
(* flat_subst corresponds to the subst_constr of this literal                 *)

let rec flatten_args
    var_table_ref x_var_list_ref flat_subst_ref args =
  match args with 
  |h::tl ->
      let next_max_var = next_top_var !x_var_list_ref in
      x_var_list_ref :=  next_max_var::(!x_var_list_ref);
      (match h with
      |Term.Var(v,_) -> 
	  if (VarMap.mem v !var_table_ref)
	  then 
	    flat_subst_ref:= (next_max_var,h)::(!flat_subst_ref)
	  else 
	    var_table_ref:=VarMap.add v next_max_var !var_table_ref
      |Term.Fun _ -> 
	  flat_subst_ref:= (next_max_var,h)::(!flat_subst_ref)
      );
      flatten_args
	var_table_ref x_var_list_ref flat_subst_ref tl
  |[] -> ()
      

let normalise_flat_subst var_table_ref max_var_ref y_var_list_ref flat_subst_ref =
  let rec f acc flat_subs = 
    match flat_subs with 
    |(v,t)::tl -> 
	let new_t =  
	  TermDB.add_ref  
	    (normalise_term_var' var_table_ref max_var_ref y_var_list_ref t) term_db_ref in
	f ((v,new_t)::acc) tl
    |[] -> acc
  in
  flat_subst_ref:= (f [] !flat_subst_ref)

(*
let normalise_clause var_table_ref lit_list = 
  let 
  normalise_term_var' var_table_ref max_var_ref var_list_ref term
*)

(*let normalise_dism_constr *)

let norm_and_flatten_args
    var_table_ref max_var_ref x_var_list_ref y_var_list_ref flat_subst_ref args =
  
  flatten_args
    var_table_ref x_var_list_ref flat_subst_ref args;
  flat_subst_ref:=List.rev !flat_subst_ref;
  max_var_ref := (next_top_var !x_var_list_ref);
  normalise_flat_subst var_table_ref max_var_ref y_var_list_ref flat_subst_ref;
(* we need to reverse flat_subst_ref twice since the first time we need the right order for the renaming *)
  flat_subst_ref:=List.rev !flat_subst_ref; 

  x_var_list_ref:=List.rev !x_var_list_ref;
  y_var_list_ref:=List.rev !y_var_list_ref

(*
let norm_clause 
let norm_dism_constr 
*)
(*
let norm_and_flatten_atom 
    var_table_ref x_var_list_ref y_var_list_ref flat_subst_ref atom = 
  match atom with
  |Term.Fun(sym,args,_) ->
      let new_args = 
	norm_and_flatten_args 
	  var_table_ref x_var_list_ref y_var_list_ref flat_subst_ref args in
      Term.create_fun_term_args sym new_args 
  |Term.Var _ -> 
      failwith "model_inst: norm_and_flatten_atom here should be a predicate\n"
*)
	
let extend_model clause model = 
  let sel_lit   = (Clause.get_inst_sel_lit clause) in
  let is_neg_lit = (Term.is_neg_lit sel_lit) in 
  let sel_atom  = (Term.get_atom sel_lit) in
  let var_table_ref = ref VarMap.empty  in
  let x_var_list_ref  = ref [] in   
  let y_var_list_ref  = ref [] in   
  let flat_subst_ref  = ref [] in
  let max_var_ref     = ref (Var.get_first_var ()) in
  match sel_atom with
  |Term.Var _-> failwith "model_inst: extend_model atom should not be a var"
  |Term.Fun(sym,args,_) ->
      norm_and_flatten_args 
	var_table_ref  
	max_var_ref 
	x_var_list_ref 
	y_var_list_ref 
	flat_subst_ref 
	(Term.arg_to_list args);
      let var_term_list = List.map Term.create_var_term !x_var_list_ref in
      let new_flat_atom = 
	(* if the symbol is equality it is replaced by symb_iprover_eq in order to avoid clash *)
        (* with "=" in formulas which is interpreted over the term algebra                     *)
	(* we do not change the symbol in normalised clauses since they do not participate in the definition *)
	let new_flat_sym = 
	  if (sym == Symbol.symb_equality)
	  then 
	    Symbol.symb_iprover_eq
	  else 
	    sym
	in
	TermDB.add_ref (Term.create_fun_term new_flat_sym var_term_list) term_db_ref in      
      let new_compl_flat_atom = TermDB.add_ref (Term.compl_lit new_flat_atom) term_db_ref in
      let (new_flat_lit, new_compl_flat_lit) =	
	if (is_neg_lit) 
	then 
	  (new_compl_flat_atom,new_flat_atom) 
	else 
	  (new_flat_atom ,new_compl_flat_atom)
      in
(* this is the subsitution constraint x_1=t_1 ,.., x_n=t_n *)
      let subst_constr = 
	{flat_subst      = !flat_subst_ref;
	 flat_subst_vars = !y_var_list_ref
       }
      in
(*--------------------Normalising clause (order literals selected first, *) 
(* apply the inverse of flattening and rename the rest of variables )---------*)
(* apply flat_subst to flat atom  *)

      let new_norm_atom = 
	let norm_args = List.map 
	    (fun v -> 
	      try 
		List.assoc v !flat_subst_ref
	      with 
		Not_found -> 
		  Term.create_var_term v  
	    ) !x_var_list_ref
	in 
	TermDB.add_ref (Term.create_fun_term sym norm_args) term_db_ref
      in
       let norm_lit = 
	if (is_neg_lit) 
	then 
	  TermDB.add_ref (Term.compl_lit new_norm_atom) term_db_ref
	else 
	  new_norm_atom
      in      
      let lits = Clause.get_literals clause in
      let rest_lits = List.filter (fun l -> not (l == sel_lit)) lits in
      let max_var_clause_ref = ref !max_var_ref in
      let norm_rest_lits =
	let dummy_y_var_list_ref = ref [] in
	List.map 
	  (fun t -> 
	    TermDB.add_ref 
	      (normalise_term_var' var_table_ref max_var_clause_ref dummy_y_var_list_ref t) 
	      term_db_ref
	  ) rest_lits in
      let norm_lits = norm_lit::norm_rest_lits in      

(*------------------------End normalising the clause--------------*)

(*-------------------Normalising dism. constraints---------------------*)
      let nomr_dism_constr dism_constr = 
(* first we need to normalise free vars: i.e t/x1, ..t_n/x_n we need to normalise x_i                                 *)
(* then order subst according the new varible order, then normalise varibles in t_i (independantly of var_table_ref)  *)
	let dc_norm_top_vars = 
	  List.map (fun (v,t) -> 	    
	    try
	      ((VarMap.find v !var_table_ref),t)  
	    with 
	      Not_found -> failwith "model_inst: all vars should have been defined in var_table_ref" 
		   ) dism_constr
	in
	let dc_ordered_top_vars = 
	  List.sort (fun (v1,_) (v2,_) -> Var.compare v1 v2) dc_norm_top_vars 
	in
	(* this is the new clause representation of the dism. constr.*)
	let dc_clause_rep = dc_ordered_top_vars in
 (* for using dism in the model                                    *)
 (* we eliminate renaming part: y_i/x_i where y_i occurs only once *)
 (* for using dism in the clause we do not eliminate the renaming part (better reflect the implementation side)*)
	let single_var_table_ref = ref VarMap.empty in 
	let non_single_var_table_ref = ref VarMap.empty in 
	let rec s_var_term t = 
	  match t with 
	   |Term.Var (v,_) -> 
	       if (VarMap.mem v !non_single_var_table_ref) 
	       then ()
	       else 
		 if (VarMap.mem v !single_var_table_ref) 
		 then 
		   (
		    single_var_table_ref     := VarMap.remove v !single_var_table_ref;
		    non_single_var_table_ref := VarMap.add v v !non_single_var_table_ref
		   )
		 else
		   (single_var_table_ref:= VarMap.add v v !single_var_table_ref)
	   |Term.Fun (sym, args,_)->
	       Term.arg_iter s_var_term	args
	in		 
	List.iter (fun (_v,t) -> s_var_term t) dc_ordered_top_vars;
	let dc_removed_renamings = 
	  let rec f rest ((_,t) as e) = 
	    match t with 
	    |Term.Var (v,_) -> 
	       if (VarMap.mem v !single_var_table_ref) 
	       then
		 (*it is a renaming -- remove*)
		 rest 
	       else
		 e::rest
	    |Term.Fun _ -> e::rest 
	  in
	  List.rev (List.fold_left f [] dc_ordered_top_vars)
	in
(*!!!!!!! if t/x and x not in x_vars U y_vars than the empty constraint!!!!!!*)
	let dc_checked = 
	 if (List.exists 
	       (fun (v,_t) -> 
		 ((not (List.mem v !x_var_list_ref)) &&  (not (List.mem v !y_var_list_ref))) 
	       )
	    ) dc_removed_renamings
	 then []
	 else   
	   dc_removed_renamings
	in
	(* no we rename defined varibles in dc (vars in t_i) *)
	let max_var_bound_dc_ref = ref !max_var_ref in
	let dc_y_var_list_ref = ref [] in
	let dc_model_rep = 	   
	  List.map 
	    (fun (v,t) ->
	      (v,
	       TermDB.add_ref 
		 (normalise_term_var' var_table_ref max_var_bound_dc_ref dc_y_var_list_ref t) 
		 term_db_ref
	      )
	    ) dc_checked
	in
(* we store vars as well in dc_model_rep:  flat_subst_vars *)
	let dc_model_rep_vars = 
	  {
	   flat_subst_vars = !dc_y_var_list_ref;
	   flat_subst = dc_model_rep;
	 }
	in
(* first is clause representation and second is the model representation *)
	(dc_clause_rep,dc_model_rep_vars)
      in
(*------------------nomr_dism_constr finished---------------*)
      let dism_constr_list = 
	try 
	  Dismatching.to_flat_subst_list_constr_set (Clause.get_dismatching clause) 
	with
	  Clause.Dismatching_undef -> []
      in
      let dc_list_clause_rep_ref      = ref [] in
      let dc_list_model_rep_vars_ref  = ref [] in
      List.iter 
	(fun dc -> 
	  let (dc_clause_rep,dc_model_rep) = nomr_dism_constr dc in 
	  dc_list_clause_rep_ref:=dc_clause_rep::(!dc_list_clause_rep_ref);
	  (if not (dc_model_rep.flat_subst = []) 
	  then 
	    dc_list_model_rep_vars_ref:= dc_model_rep::(!dc_list_model_rep_vars_ref)
	  )
	) dism_constr_list;
      
(*------------------------------Filling the Model----------------------------*)

      let renew_lit_def_body lit_def = 
	let new_model_lit_def_body = 
	  try 
	    let (dism_constr_set_ref, norm_clause_list_ref) = 
	      FSVMap.find subst_constr (lit_def.model_lit_def_body) 
	    in
(* a bit of useless work when DCSSubsumed we do not need to generate dc_list_clause_rep_ref *)
(* but avoiding this creates a bit of a mess so leave it for the moment                     *)

	    dism_constr_set_ref:=
	      (match !dism_constr_set_ref with 
	      |DCSSubsumed ->  DCSSubsumed
	      |DCSet dc_set ->  
		  if (!dc_list_model_rep_vars_ref = []) 
		  then DCSSubsumed
		  else
		    DCSet(
		    List.fold_left (fun set constr_vars -> FSVSet.add constr_vars set ) 
		      dc_set !dc_list_model_rep_vars_ref;
		   )
	      );

	    norm_clause_list_ref := 
	      (clause, (norm_lits,!dc_list_clause_rep_ref))::(!norm_clause_list_ref);
	    lit_def.model_lit_def_body
	  with 
	    Not_found ->
	      let dism_constr_set_ref = 
		ref 
		  (if (!dc_list_model_rep_vars_ref = []) 
		  then DCSSubsumed
		  else
		    DCSet(
		    (List.fold_left (fun set constr_vars -> FSVSet.add constr_vars set )
        	       FSVSet.empty !dc_list_model_rep_vars_ref) 
		   )
		  )
	      in
	      let norm_clause_list_ref = ref 
		  [(clause, (norm_lits,(!dc_list_clause_rep_ref)))] in 
	      
	      FSVMap.add 
		subst_constr 
		(dism_constr_set_ref, norm_clause_list_ref) 
		lit_def.model_lit_def_body
	in
	lit_def.model_lit_def_body <- new_model_lit_def_body	  	
      in	      
      try 
	let model_node = NModel.find sym model in 
	let lit_def = 
	  if is_neg_lit 
	  then 
	    model_node.neg_lit_def
	  else 
	    model_node.pos_lit_def
	in
	renew_lit_def_body lit_def;
	model
      with 
	Not_found -> 
(* definiton for the same polarity  as lit *)
	  let new_same_pol_lit_def = 
	    {
	     model_lit          = new_flat_lit;
	     model_lit_vars     = !x_var_list_ref;
	     model_lit_def_body = FSVMap.empty;

(* this will be  changed later in fill_stat_model *)
	     model_lit_undef               = false;    
	     model_lit_always_true         = false;
	     model_lit_num_of_subst_constr = 0;
	     model_lit_num_of_dism_constr  = 0;   
	   }
	  in
	  renew_lit_def_body new_same_pol_lit_def;
(* definiton for the compl polarity  as lit *)
	  let new_compl_pol_lit_def = 
	    {
	     model_lit          = new_compl_flat_lit;
	     model_lit_vars     = !x_var_list_ref;
	     model_lit_def_body = FSVMap.empty;

(* this will be  changed later in fill_stat_model *)
	     model_lit_undef               = false;	     
	     model_lit_always_true         = false;
	     model_lit_num_of_subst_constr = 0;
	     model_lit_num_of_dism_constr  = 0;   

	   }
(* we do not need to renew_lit_def_body for new_compl_pol_lit_def *)
	  in
	  let new_model_node = 
	    {
	     pos_lit_def = 
	     (if is_neg_lit 
	     then new_compl_pol_lit_def
	     else new_same_pol_lit_def); 
	     
	     neg_lit_def = 
	     (if is_neg_lit 
	     then new_same_pol_lit_def
	     else new_compl_pol_lit_def); 
	     
	   }
	  in
	  let new_model = NModel.add sym new_model_node model in 
	  new_model

	    

      (*----------debug----------*)
(*
type lit_def_body = ((dism_constr_set ref) * ((norm_clause list) ref) FSVMap.t)

type lit_def = 
    {
(* this is a flattened normalised literal of the form (~)P(x_0,..,x_n) *)
     model_lit          : lit; 
(* ordered list of vars in the literal *)
     model_lit_vars     : var list; 
     mutable model_lit_def_body : lit_def_body;     
   }

type model_node = 
   {
 (* positive and negative definitions of the same predicate *)
    pos_lit_def : lit_def; 
    neg_lit_def : lit_def; 
  } 

(* normal model is a map from predicates to model nodes definig these predicates *)
module NModel =  Map.Make(Symbol)

type norm_model = (model_node NModel.t)

*)
(*
      stdout_stream.stream_add_str "\n normalised atom:\n";
      Term.to_stream stdout_stream new_norm_atom;
      stdout_stream.stream_add_str "\n normalised lit lis:\n";
      Term.out_term_list norm_lits;
      stdout_stream.stream_add_str "\nx_var_list_ref\n";
      var_list_to_stream stdout_stream  !x_var_list_ref;
      stdout_stream.stream_add_str "\n!y_var_list_ref\n";
      var_list_to_stream stdout_stream  !y_var_list_ref;
      stdout_stream.stream_add_str "\n!flat_subst_ref\n";
      to_stream_plain_fs stdout_stream !flat_subst_ref
  |Term.Var _ -> 
      failwith "model_inst: norm_and_flatten_atom here should be a predicate\n"
*)


(* stdout_stream.stream_add_str (pref_str^"Building Model"); *)
(* returns the model *)

let test clause model = 
  out_str "\n!!!Begin Debug!!!!!\n";
  Dismatching.to_stream_constr_set stdout_stream (Clause.get_dismatching clause);
  out_str "\n!!!End Debug!!!!!\n";
  model
 
let test_iter clause = 
  if not (Clause.get_bool_param Clause.in_active clause)
  then ()
  else
    (
     out_str "\n!!!Begin Debug!!!!!\n";
     Dismatching.to_stream_constr_set stdout_stream (Clause.get_dismatching clause);
     out_str "\n!!!End Debug!!!!!\n"
    )



(*---------------- fills statistics and extras in model -------------*)
(*
     mutable model_lit_undef                  : bool;
     mutable model_lit_always_true            : bool;
     mutable model_lit_num_of_subs_constr     : int;
     mutable model_lit_num_of_dism_constr     : int; 
*)


let fill_stat_model model = 
  let fill_lit_def ld = 
    if (ld.model_lit_def_body = FSVMap.empty) 
    then 
      (
       ld.model_lit_undef                 <- true;
       ld.model_lit_num_of_subst_constr   <- 0;
       ld.model_lit_num_of_dism_constr    <- 0;
      )
    else
      begin
       let is_always_true = 
	 try 
	   let empty_subs_constr = {flat_subst_vars = []; flat_subst = []} in
	   let (dism_constr_set_ref, _norm_clause_list_ref) =  FSVMap.find empty_subs_constr ld.model_lit_def_body in
	   match !dism_constr_set_ref with 
	   |DCSet s ->  
	       if s = FSVSet.empty 
	       then true 
	       else false 
	   |DCSSubsumed -> true
	 with 
	   Not_found -> false	       
       in
       if is_always_true 
       then
	 (
	  ld.model_lit_always_true         <- true;
	  ld.model_lit_num_of_subst_constr <- 0;
	  ld.model_lit_num_of_dism_constr  <- 0;
	 )
       else
	 (
	  let f _subst_constr (dism_constr_set_ref, _norm_clause_list_ref) = 
	    ld.model_lit_num_of_subst_constr <- (ld.model_lit_num_of_subst_constr +1);
	    (match !dism_constr_set_ref with 
	    |DCSet s ->  
		ld.model_lit_num_of_dism_constr <- ld.model_lit_num_of_dism_constr + (FSVSet.cardinal s)
	    |DCSSubsumed -> ()
	    )
	    in
	  FSVMap.iter f ld.model_lit_def_body
	 )
      end
  in
  let f _symb model_node = 
    fill_lit_def model_node.pos_lit_def;
    fill_lit_def model_node.neg_lit_def
  in
  NModel.iter f model 


let build_model all_clauses = 
  stdout_stream.stream_add_str (pref_str^"Building Model..."); 
  let empty_model = NModel.empty in

(*  out_str "\n-------All clauses------\n";
  ClauseAssignDB.iter (fun c ->out_str ((Clause.to_string c)^"\n")) all_clauses;
  out_str "\n-------End All clauses------\n";
*)

  let new_model =
    ClauseAssignDB.fold
      (fun clause model ->
	if (Clause.get_bool_param Clause.in_active clause)
	then extend_model clause model       
	else model
      )
      all_clauses
      empty_model 
  in
  fill_stat_model new_model;
  stdout_stream.stream_add_str ("Done\n"); 
  new_model



type model = norm_model

(*type model = raw_model *)
(*let build_model active_clauses = active_clauses*)

(*--------------- To stream -------------------*)

let to_stream_var_list s vl = 
  s.stream_add_char '[';
  list_to_stream s Var.to_stream vl ",";
  s.stream_add_char ']'
    

(* eq_sign_str either  = or != *)
let to_stream_vt eq_sign_str s  (v,t) =  
  Var.to_stream s v;
  s.stream_add_str eq_sign_str;
  Term.to_stream s t

(* eq_sign_str either  = or !=, con_str is either "&" or "|" *)
let to_stream_fs eq_sign_str con_str s constr =  
  s.stream_add_str "( ";
  Lib.list_to_stream s (to_stream_vt eq_sign_str)  constr (" "^con_str^" ");
  s.stream_add_str " )"


(* returns (nonempty_quant, nonempty_body) in order to close brackets/add leading &  later*)
let to_stream_subs_constr s subs_constr = 
  let nonempty_quant_ref  = ref false in
  let nonempty_body_ref   = ref false in

  (if subs_constr.flat_subst_vars = [] 
  then ()
  else
    (
     nonempty_quant_ref:=true;
     to_stream_space s 12;
     s.stream_add_str "? ";
     to_stream_var_list s subs_constr.flat_subst_vars;
     s.stream_add_str " : \n";
    )
  );
  to_stream_space s 14;
  s.stream_add_str "(\n";
 
  (if (subs_constr.flat_subst = []) 
  then () (*(s.stream_add_char '\n')*)
  else
    (nonempty_body_ref := true;
    to_stream_space s 16;
    to_stream_fs "=" "&" s subs_constr.flat_subst
    )
  );
 (!nonempty_quant_ref, !nonempty_body_ref)
(*
  (if subs_constr.flat_subst_vars = [] 
  then ()
  else 
    to_stream_space s 15;
    s.stream_add_str ")\n"  (* (? end should be after all dism constr *)
  )
*)    


let to_stream_dism_constr_set exists_constr_before s dc_set = 
(* FSVSet.empty never happens since we would add [] constr which would simplified to DCSSubsumed *)
  if (dc_set = FSVSet.empty)
  then 
    (to_stream_space s 15;  
     s.stream_add_str "$true";)
  else
    begin
      let is_first_dc = ref (not exists_constr_before) in 
      let f dc = 
	(if (not !is_first_dc) 
	then 
	  (s.stream_add_str "\n";
	   to_stream_space s 15; 
	   s.stream_add_str "&\n";)
	else 		  (is_first_dc:= false;)
	);

       to_stream_space s 16;
       (if dc.flat_subst_vars = [] 
       then ()
       else
	 (
	  s.stream_add_str "! ";
	  to_stream_var_list s dc.flat_subst_vars;
	  s.stream_add_str " : ";
	 )
       );
	(if (dc.flat_subst = []) 
	then ()
	else
	  (
	   to_stream_fs "!=" "|" s dc.flat_subst
	  )
	); 
     in
     FSVSet.iter f dc_set
    end

let to_stream_lit_def_body s opt lit_def_body = 
(* since we fill_stat_model lit_def_body = FSVMap.empty would result to          *)
(* ld.model_lit_undef                 <- true; and would be considered before in  to_stream_lit_def *)
  assert (not (lit_def_body = FSVMap.empty));
  begin    
    to_stream_space s 11; 
    s.stream_add_str "(\n"; (* (.. | ..)*)
(*  if it is not first then we add \n | \n before the constr *)
      let is_first_subs_constr = ref true in
	  
	  let f subs_constr (dism_constr_set_ref, norm_clause_list_ref) = 
	    (
	     (if (not !is_first_subs_constr) 
	     then 
	       (s.stream_add_char '\n';
		to_stream_space s 12; 
		s.stream_add_str " | \n";)
	     else 	
	       (is_first_subs_constr:= false;)
	     );

	     let  (nonempty_quant, nonempty_body) = to_stream_subs_constr s subs_constr in
	     (
	      match !dism_constr_set_ref with 
	      |DCSet dc_set ->
		  to_stream_dism_constr_set nonempty_body s dc_set;
	      |DCSSubsumed -> ()
	     );
        
	(* FSVMap.t *)
	(* ( if nonempty_quant
	    then *)
	     (s.stream_add_char '\n';
	      to_stream_space s 14;
	      s.stream_add_str ")\n";  (* end \(? from subs_constr *)
	     );
	     (if (opt = Model_Debug) 
	     then 
	       to_stream_plain_norm_clause_list s !norm_clause_list_ref
	     else ()
	     )
	       
	       (*else ()
		 );*)
	    )
     in
     FSVMap.iter f lit_def_body;
     s.stream_add_char '\n';
     to_stream_space s 11; 
     s.stream_add_str ")\n"; (* end (.. | ..)*)       	
    end

(* con_str if either "<=>"  or "<=" equivalence or implication definition *)
let def_type_to_con_str def_type = 
  match def_type with 
  |Def_Equiv -> "<=>" 
  |Def_Implied -> "<="

let to_stream_lit_def s opt def_type lit_def = 
(*
  out_str ("\n%--- model_lit_undef:"^(string_of_bool lit_def.model_lit_undef)^"\n");
  out_str ("\n%--- lit_def.model_lit_always_true: "^(string_of_bool lit_def.model_lit_always_true)^"\n");
  out_str ("\n%--- lit_def.model_lit_num_of_subs_constr : "^(string_of_int lit_def.model_lit_num_of_subst_constr)^"\n");
  out_str ("\n%--- lit_def.model_lit_num_of_dism_constr : "^(string_of_int lit_def.model_lit_num_of_dism_constr)^"\n");
*)

(* do not need to output anything if "<=" and lit was not defined  *)
(* if "<=>" and lit is not defined we need "L <=> $false "         *)
  if (def_type = Def_Implied) && (lit_def.model_lit_undef)
  then (s.stream_add_str (model_pref_str^"Empty\n" ))
  else
    begin
      s.stream_add_str "fof(lit_def,axiom,\n";
(*---! [X1,...,Xn] : \n-----*)
      (if (lit_def.model_lit_vars = []) 
      then ()
      else
	(to_stream_space s 4;
	 s.stream_add_str "(! ";
	 to_stream_var_list s lit_def.model_lit_vars;
	 s.stream_add_str " : \n";
	)
      );
(*--------(L(X1,..,Xn) <=>/<=\n----*)
      to_stream_space s 6;
      s.stream_add_str "( ";
      Term.to_stream s lit_def.model_lit;
      s.stream_add_str (" "^(def_type_to_con_str def_type)^"\n");

(*----definition body------*)
      (if lit_def.model_lit_always_true 
      then 
	(
	 if (not (opt = Model_Debug))
	 then
	   (
	    to_stream_space s 10;
	    s.stream_add_str "$true\n";
	   )
	 else
	   (
	    to_stream_space s 10;
	    s.stream_add_str "% $true\n";
	    to_stream_lit_def_body s opt lit_def.model_lit_def_body;
	   )
	 )	
      else 
	(if lit_def.model_lit_undef 
	then 
	  ( assert (not (def_type = Def_Implied));
	    to_stream_space s 10;
	    s.stream_add_str "$false\n"
	   )
	else
	  (
	   to_stream_lit_def_body s opt lit_def.model_lit_def_body;	)
	)
      );      
      to_stream_space s 6; s.stream_add_str ")\n"; (* end (L(..) ..*)
      
      (if (lit_def.model_lit_vars = []) 
      then ()
      else  
	(to_stream_space s 4; s.stream_add_str ")\n";) (* end (! *)
      );
      
      to_stream_space s 3; s.stream_add_str ").\n" (* end fof(apt_def,axiom,( *)
    end
 (*
     model_lit          : lit; 

     model_lit_vars     : var list; 
     mutable model_lit_def_body : lit_def_body;     
 
  *)

let to_stream_model s opt model =
  if (opt = Model_None)   
  then ()
  else
    (
     let def_type = 
       match opt with
       |Model_Pos | Model_Neg | Model_Small -> Def_Equiv 
       |Model_Implied |Model_Debug          -> Def_Implied                              
       |Model_None                          -> failwith ("model_inst: Model_None")
     in
     s.stream_add_char '\n';
     s.stream_add_str 
       (model_pref_str^"The model is defined over ground terms (initial term algebra).\n"^
	model_pref_str^"Predicates are defined as (\\forall x_1,..,x_n  ((~)P(x_1,..,x_n) "^(def_type_to_con_str def_type)^" (\\phi(x_1,..,x_n)))) \n"^
	model_pref_str^"where \\phi is a formula over the term algebra.\n"^
  	model_pref_str^"If we have equality in the problem then it is also defined as a predicate above, \n"^
	model_pref_str^"to avoid the clash with \"=\" in the denfitions (interpreted over the term algebra) it is renamed to "
	^(Symbol.to_string Symbol.symb_iprover_eq)^"\n"^
	model_pref_str^"See help for --sat_out_model for different model outputs.\n");
     s.stream_add_str "\n\n% SZS output start Model \n\n";


     let f key_sym model_node = 
       s.stream_add_char '\n';

       let (out_pos,out_neg) = 
	 match opt with 
	 |Model_Pos   -> (true,false)	 
	 |Model_Neg   -> (false,true)
	 |Model_Small ->
	     let pos_score = 
	       model_node.pos_lit_def.model_lit_num_of_subst_constr 
		 + model_node.pos_lit_def.model_lit_num_of_dism_constr 
	     in
	     let neg_score = 
	       model_node.neg_lit_def.model_lit_num_of_subst_constr 
		 + model_node.neg_lit_def.model_lit_num_of_dism_constr 
	     in
	     if (pos_score <= neg_score) 
	     then (true,false)
	     else (false,true)
	 |Model_Implied |Model_Debug -> (true,true)
	 |Model_None  -> failwith ("model_inst: Model_None")
       in
       (if out_pos
       then
	 (s.stream_add_str (model_pref_str^"Positive definition of " 
			   ^(Symbol.to_string key_sym)^" \n" );	   
	to_stream_lit_def s opt def_type model_node.pos_lit_def;)
       else ()
	);
       (
	if out_neg
	then 
	  (s.stream_add_str (model_pref_str^"Negative definition of "
			     ^(Symbol.to_string key_sym)^" \n" );
	   to_stream_lit_def s opt def_type model_node.neg_lit_def;
	  )
	else ()
       )     
     in
     NModel.iter f model;
     s.stream_add_str "\n\n% SZS output end Model \n\n"    
    )


let out_model model = 
  to_stream_model stdout_stream !current_options.sat_out_model model



(*


type dism_constr_set = 
  |DCSet of FSVSet.t
(* Subsumed means that all dismatching constraints are subsumed     *)
(* by dismatching constr which is either undefined of is a renaming *)
(* (over all vars in the clause)           *)
(* so L(X)| Subsumed means \forall X L(X) (in other notation \forall X (L(X) <-> true) ) *)
  |DCSSubsumed

type var_table = (var VarMap.t)

type var_table_ref = var_table ref

(*we pair old clause with the normalised lit list form this clause and constraint list*)
type norm_clause = 
    (clause * ((lit list) * flat_subst list))
(* literal definition body is a map from subst_constraints into sets of dismatching constrints *)
(* paired with a list of clauses where the L\subst_constraint is selected  *)
(* dismatching constraints are normalised and only varibales in L are left *)
(*  clauses are also normalised such that L is first, vars renamed, including in dismatiching constr.*)

type lit_def_body = (((dism_constr_set ref) * ((norm_clause list) ref)) FSVMap.t)

type lit_def = 
    {

(* this is a flattened normalised literal of the form (~)P(x_0,..,x_n)                       *)
     model_lit          : lit; 

(* ordered list of vars in the literal                                                       *)
     model_lit_vars     : var list; 

(* a) if lit_def_body is FSVMap.empty then this literal was not defined and this corresponds to          *)
(* \forall X ~L(X)    or \forall X (L(X) <-> $false)                                                     *)
(* b) if [] is a key in  lit_def_body, this corresponds to the empty subst_constr, there are still       *)
(*    can be dismatching constraints corresponding to []                                                 *)
     mutable model_lit_def_body : lit_def_body;     
   }
*)
(* debug *)

(*
let out_model active_clauses =
   let f clause = 
    if not (Clause.get_bool_param Clause.in_active clause)
    then ()
    else
      begin
	stdout_stream.stream_add_str "%---------------\n";
	Clause.to_stream stdout_stream clause;
	stdout_stream.stream_add_char ' ';
	Term.to_stream stdout_stream (Clause.get_inst_sel_lit clause);
	stdout_stream.stream_add_char '\n';
	(try 	
	  Dismatching.to_stream_constr_set stdout_stream (Clause.get_dismatching clause);
	  stdout_stream.stream_add_char '\n';
	with 
	  Clause.Dismatching_undef ->
	    stdout_stream.stream_add_str "[]\n");	
	stdout_stream.stream_add_str "%----Debug-----------\n";
(*	extend_model model clause; *)
	stdout_stream.stream_add_char '\n';
      end
  in
  ClauseAssignDB.iter f active_clauses;
  stdout_stream.stream_add_str "\n%---------------End Model--------------\n\n"

*)
