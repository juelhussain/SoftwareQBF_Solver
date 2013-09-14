(** The main program. *)
   
module ProcessInput = 
	struct


	
	let cycleString st = 
		let rec cycle i = 
			if i> (String.length st)-2 then print_string (st^"\n") else
			if ((String.make 1 st.[i])^(String.make 1 st.[i+1]) = "-c"
				||(String.make 1 st.[i])^(String.make 1 st.[i+1]) = "-d") 
				then begin Printf.printf "QBF_Solver- Option: %s\n" ((String.make 1 st.[i])^(String.make 1 st.[i+1])); 
				cycle (i+1) end
			else cycle (i+1)
		in cycle 0

 	let stripOption string option =
      let len = String.length string in
      let rest = String.create len in
      let rec cycle1 i j =
        if i >= len then String.sub rest 0 j
        else if String.contains option string.[i] then
          cycle1 (i+1) (j)
        else begin
          rest.[j] <- string.[i];
          cycle1 (i+1) (j+1)
        end
      in
      cycle1 0 0
	
	let processString str =
  let rec cycle st i optionList = 
				if i> (String.length st)-2 then 
					begin 
						print_string (st^"\n"); 
						 (stripOption st "-c-d")::optionList  
					end 
				else
					let first = (String.make 1 st.[i]) in
					let second = (String.make 1 st.[i+1]) in
    			if ((first^second = "-c")||(first^second = "-d")) 
    				then 
							begin 
								Printf.printf "QBF_Solver- Option: %s\n" (first^second); 
    						cycle st (i+1) ((first^second)::optionList)
							end
    			else cycle st (i+1) optionList
  		in cycle str 0 []

end;;
