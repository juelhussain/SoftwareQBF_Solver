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
open Printf
open Parser_types
open Options

let parser_main_fun = Parser_tptp.main
let lexer_token = Lexer_tptp.token

(* when clausifier is used (the propblem is in fof), includes are assumed to be unfolded by the clausifier *)


(* include path ex:  "/home/TPTP-v3.0.1/Problems/"  include('Axioms/SET003-0.ax').*)
let remove_double_quotes str =
  let str_ln = String.length str in
  if str_ln > 1 then
    if (str.[0] = '\"') & (str.[(str_ln-1)] = '\"')
    then 
      if  str_ln > 2 
      then 
	String.sub str 1 (str_ln-2) 
      else ""
    else str
  else str



(*--------------------------*)

let append_iprover_path file_name =
  try 
    Filename.concat program_path file_name
  with 
    Not_found -> 
      Filename.concat "./" file_name

let clausifier_cmd_options () = 

(* find clausifier in the following sequence: *)
(* 1) --clausifier if not "" then take clausifier options from --clausifier_options *)
(* 2) environment vars CLAUSIFIER and CLAUSIFIER_OPTIONS *)
(* 3) from iprover_dir/VClausifier/vclausify_rel with  vlclausifier_default_options*)
(* 4) form iprover_dir/E_Prover/eprover with eprover_default_options *)
(*    iprover_dir is taken as Unix.readlink /proc/self/exe *)

  let vlcausify_rel_cmd           = append_iprover_path "VClausifier/vclausify_rel" in
  let eprover_cmd                 = append_iprover_path "E_Prover/eprover" in
  let clausifier_env_name         = "CLAUSIFIER" in
  let clausifier_env_options_name = "CLAUSIFIER_OPTIONS" in
  
  let cpu_limit = 
    if input_options.time_out_virtual > 0. then
      (int_of_float input_options.time_out_real)+1 
    else 
      if input_options.time_out_real > 0. then
	(int_of_float input_options.time_out_real)+1
      else
	0
  in  
  let default_vclausify_rel_options  = 
    " --mode clausify "
    ^(if cpu_limit > 0 then ("-t "^(string_of_int cpu_limit)) else "")
  in
  let default_eprover_options = 
    " --tstp-format --free-numbers --free-objects --split-method=1  --silent --cnf "^
    (if cpu_limit > 0 then (" --cpu-limit="^(string_of_int cpu_limit)) else "")
  in
  
  let check_clausifier cmd = 
    if cmd = "" 
    then
      false
    else
      if (Sys.file_exists cmd)
      then 
	true 
      else
	failwith ("cannot find clausifier: "^cmd^
		  ", please specify an appropriate --clausifier")    
  in
  let (cmd, options) = 
    if (check_clausifier !current_options.clausifier)
    then 
      (!current_options.clausifier, !current_options.clausifier_options)
    else
      let cmd_env_name = 
	try 
	  remove_double_quotes (Unix.getenv clausifier_env_name)
	with Not_found -> ""
      in
      if (check_clausifier cmd_env_name)
      then
	let options =
	  try 
	     remove_double_quotes (Unix.getenv clausifier_env_options_name)
	  with
	    Not_found -> ""
	in 
	(cmd_env_name, options)
      else	
	if (Sys.file_exists vlcausify_rel_cmd) 
	then
	  (vlcausify_rel_cmd, default_vclausify_rel_options)
	else
	  if (Sys.file_exists eprover_cmd)
	  then 
	    (eprover_cmd, default_eprover_options)
	  else
	    (failwith 
	      ("cannot find clausifier, please specify using --clausifier and --clausifier_options"))
	    
  in    	    
  (cmd,options)


(*-------------------------------------------*)

let check_clausifier_error_channel error_channel =   
  ()

(* We ignore output into the error_channel for now *)

(* OLD checks of error_channel for eprover, now all ignored, we just check the exit status 

  (* Ignore warnings *)
  let ignore_regexp = Str.regexp "eprover: Warning: " in

  (* Save lines read from stderr *)
  let error_line = ref [] in
  try 
    
    (* Read from stderr until end *)
    let rec f () = 
	
	(* Read one line *)
      let add_line = input_line error_channel in      
      if 
	
	    (* Error is only a warning? *)
	Str.string_match ignore_regexp add_line 0	  
      then
	    
	    (* Ignore line *)
	(
	 Printf.printf "Ignoring \"%s\"\n%!" add_line
	)	  
      else	
	(
	 
	      (* Kepp error line *)
	 error_line := (add_line)::(!error_line)
	);

	  (* Loop until end of file *)
	  f ()
    in 
    (* Read all lines from stderr *)
    f ()
	  
  with End_of_file -> 
    ( 	
       if 

	  (* No error messages on stderr? *)
	  !error_line = [] 
	then 
	  (* Continue *)
	  ()
	else 
	  (* Fail *)
	  (out_str "\n\n# SZS status: Unknown\n"; 		
	   failwith ("fail to clausify by E prover: "
		     ^(String.concat "\n" (List.rev !error_line)))
	  )
      )
*)

let check_clausifier_process_status process_status =
  match process_status with 
(*	The process terminated normally by exit; the argument is the return code.	*)
  | 	Unix.WEXITED int  -> 
      if int = 0 then ()
      else 
	failwith ("Clausification error: "^(!current_options.clausifier)^" exits with an error status: "
		  ^(string_of_int int))

(*	The process was killed by a signal; the argument is the signal number.	*)
  | 	Unix.WSIGNALED int -> 
      failwith ("Clausification error: "^(!current_options.clausifier)^" prover was killed by a signal: "
		  ^(string_of_int int))
	(*	The process was stopped by a signal; the argument is the signal number.	*)
  | 	Unix.WSTOPPED int ->
      failwith ("Clausification error: "^(!current_options.clausifier)^" was stopped by a signal: "
		^(string_of_int int))

let get_line_num_lexbuf lexbuf = 
  let position = (Lexing.lexeme_end_p lexbuf) in
  let line_number = position.Lexing.pos_lnum in
  line_number


exception Parsing_fails_ln of int
(* when parsing fails raise "Parsing_fails_ln n" *)
(* where n is the current line number in the     *)

 
(* can raise Parsing_fails_ln*)
let parse_lexbuf lexbuf = 
  let () = init_lexbuf lexbuf in
  try 
    (parser_main_fun lexer_token lexbuf) 
  with 
    Parsing_fails -> 
      raise (Parsing_fails_ln (get_line_num_lexbuf lexbuf))
	





(* clausify_parse_channel can raise Parsing_fails_ln *)

let clausify_parse_channel clausifier_full_cmd in_channel = 

  let env = Unix.environment () in  

  let cl_out_pipe_out, cl_out_pipe_in = Unix.pipe () in
  let cl_out_pipe_out_ch = Unix.in_channel_of_descr  cl_out_pipe_out in 
(* won't need *)
  let _cl_out_pipe_in_ch  = Unix.out_channel_of_descr cl_out_pipe_in in 

(*
  let cl_err_pipe_out, cl_err_pipe_in = Unix.pipe () in
  let cl_err_pipe_out_ch = Unix.in_channel_of_descr cl_err_pipe_out in 
*)

(* won't need *)
(*  let _cl_err_pipe_in_ch  = Unix.out_channel_of_descr cl_err_pipe_in in *)

(* add redirection of cl_error into error_channel *)

  let cmd_args_list 
      =  Str.split (Str.regexp "[ ]+") (clausifier_full_cmd) in
  let cmd_args = Array.of_list cmd_args_list in
  let cmd_name = cmd_args.(0) in

  let in_dscr = (Unix.descr_of_in_channel in_channel) in

  prerr_newline ();
  let cl_pid = 
    Unix.create_process_env cmd_name cmd_args env
      in_dscr cl_out_pipe_in Unix.stderr (*cl_err_pipe_in*)
  in

  prerr_newline ();
(* cat_pid is used for testing: just redirects input into output *)
(*
  let cat_pid = 
    Unix.create_process_env "cat" (Array.of_list ["cat"]) env
      (Unix.descr_of_in_channel in_channel)
     e_out_pipe_in e_err_pipe_in
  in
*)

(* ! We need to close this end of pipe since it is copyied to the process !*)
(* Otherwise EOF is not sent which creats a bolck*)
  Unix.close in_dscr;
  Unix.close cl_out_pipe_in;
(*  Unix.close cl_err_pipe_in;*)

  add_child_process cl_pid;

  let lexbuf = Lexing.from_channel cl_out_pipe_out_ch in 
(*  flush e_out_pipe_in_ch;*)
  let clause_list = parse_lexbuf lexbuf in
  let cl_pid_, cl_status = Unix.waitpid [Unix.WUNTRACED] cl_pid in 
(*  check_clausifier_error_channel stderr cl_err_pipe_out_ch;*)
  Unix.close cl_out_pipe_out;
  check_clausifier_process_status cl_status;
  clause_list 
  

(*------------------------------------------------*)

let clausify_parse problem_files =
  let (clausifier_cmd,options) = clausifier_cmd_options ()  in
  let clausifier_full_cmd = clausifier_cmd^" "^options in
  let clausifier_short_name = Filename.basename clausifier_cmd in

  print_string 
    ("\n"^pref_str^"Clausification by "^clausifier_short_name^"  & Parsing by iProver");
  flush stdout;
  (   
      try 
	
	(* Check if environment variable set *)
	ignore (Unix.getenv "TPTP")	  
      with Not_found ->
	
        (* Pass include path on to E via $TPTP *)
  	Unix.putenv "TPTP" !current_options.include_path	  
     );
  
  let result =    
    if !current_options.stdin then
      ( print_string " from stdin...";
	flush stdout;
	try
	  clausify_parse_channel clausifier_full_cmd stdin
	with 
	|Parsing_fails_ln line_number -> 
	    let fail_str = 
	      "Parse error from clausified stdin: "
	    ^" line: "^(string_of_int line_number) in 
            failwith fail_str
       )    
    else
      (print_string "...";
       flush stdout;    
       let for_one_file rest file_name = 
	 let full_file_name = 
	   (Filename.concat 
	      (remove_double_quotes !current_options.problem_path) file_name)
	 in
	 let in_channel = open_in  full_file_name in
	 try
	   let clause_list = 
	     clausify_parse_channel clausifier_full_cmd in_channel in
	   clause_list@rest 
	 with  
	 |Parsing_fails_ln (line_number) -> 
	     let fail_str = "Parse error line: "^(string_of_int line_number) in 
             failwith fail_str
     in
       List.fold_left for_one_file [] problem_files
      )
  in
  out_str (pref_str^"Parsing Successful");
  result

(*------------- end clausifier version ----------------*)


(* parse_channel can raise (Parsing_fails_ln line_number) *)

let parse_channel in_channel = 
   let lexbuf = (Lexing.from_channel in_channel) in
   parse_lexbuf lexbuf

	    
    
(* parsers without unfolding includes *)
let parse_list file_name_list= 
  let parse_one_file rest file_name =
    let in_channel = open_in file_name in
    let add_parsed_list=      
      try 
	parse_channel in_channel
      with 
	 |Parsing_fails_ln line_number -> 
	     let ()= close_in in_channel in
	     let fail_str = "Parse error in file: "
	       ^file_name^" line: "^(string_of_int line_number) in 
             failwith fail_str
    in
    let ()= close_in in_channel in
    add_parsed_list@rest 
  in
  List.fold_left parse_one_file [] file_name_list

  


(* 
let parsed_input_files = parse_list options.problem_files
*)
 
  
(* circular inputs detected and added once*)
let rec unfold_includes' include_path parsed_list added_files_ref= 
  match parsed_list with
  |Include(file_name,[])::tl ->  
      let full_file_name = Filename.concat include_path file_name in  
      if List.mem full_file_name !added_files_ref 
      then (unfold_includes' include_path tl added_files_ref)
      else
	(added_files_ref:= full_file_name::!added_files_ref;
      let add_parsed_list = parse_list [full_file_name] in 
 (*         out_str file_name; *)
          unfold_includes' include_path (add_parsed_list@tl) added_files_ref)
	  
  |Include(_,_::_)::tl ->  	  
       failwith "formula selection in include is not supported yet"
  |h::tl-> h::(unfold_includes' include_path tl  added_files_ref)
  |[]->[]
	
let unfold_includes include_path parsed_list = 
  let added_files_ref = ref [] in
  unfold_includes' include_path parsed_list added_files_ref

(*
let parsed_all = unfold_includes parsed_input_files
*)

let parse_files include_path file_name_list = 
  print_string (pref_str^"Parsing");
  flush stdout;
  let parsed_without_includes = 
    if !current_options.stdin then
      ( print_string " from stdin...";
	 flush stdout;
	try
	  parse_channel stdin      
	with 
	|Parsing_fails_ln line_number -> 
	    let fail_str = 
	      "Parse error from stdin: "
	      ^" line: "^(string_of_int line_number) in 
            failwith fail_str
	|Parser_types.FOF_format ->	    
   	     failwith "fof format is detected in stdin but \"--fof false\" is specified, you can try --fof true option"
       )    
    else
     (print_string "...";
      flush stdout;
      let full_names_list = 
	List.map 
	  (fun x -> 
	    Filename.concat 
	      (remove_double_quotes !current_options.problem_path) x)
	  file_name_list in
      parse_list full_names_list
      )
  in
  let parsed= unfold_includes include_path parsed_without_includes in
  out_str "successful\n";
  parsed

    
(*
let all_in_str = parsing_type_to_string  parsed_all
let () = 
  out_str_debug ("Parsed formulas: \n"^all_in_str^"end of parsed formulas \n") 
*)

(*------------------------Commented----------------------*)
