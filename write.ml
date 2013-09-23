open Printf



let write file message =
  (* Write message to file *)
  let oc = open_out file in    (* create or truncate file, return channel *)
  fprintf oc "%s\n" message;   (* write something *)   
  close_out oc;                (* flush and close the channel *)

  (* Read file and display the first line *)
  let ic = open_in file in
  try 
    let line = input_line ic in  (* read line from in_channel and discard \n *)
    print_endline line;          (* write the result to stdout *)
    flush stdout;                (* write on the underlying device now *)
    close_in ic                  (* close the input channel *) 

  with e ->                      (* some unexpected exception occurs *)
    close_in_noerr ic;           (* emergency closing *)
    raise e                      (* exit with error: files are closed but
                                    channels are not flushed *)

  (* normal exit: all channels are flushed and closed *)
	
	
	
	let rec next j clause_size element =
		if (j>=(List.length element)) then clause_size
		else next (j+1) (clause_size+1) element
		
		
	
	let get_number_of_clauses_list (inputBDDlist_list) =
		let rec next_bdd i clause_size = 
			if (i>=(List.length inputBDDlist_list)) then clause_size 
			else
			begin
				let element = (List.nth inputBDDlist_list i) in
					let clause_size = next 0 clause_size element in
				next_bdd (i+1) (clause_size)
			end
		in next_bdd 0 0
	
	
	let get_number_of_clauses (inputBDDlist) =
		let rec next i clause_size = 
			if (i>=(List.length inputBDDlist)) then clause_size 
			else
			begin
  			next (i+1) (clause_size +1)
			end
		in next 0 0
		
		
		
		
		
		
		
		