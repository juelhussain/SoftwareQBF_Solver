
module Conjunction = 
	struct
	(* This gives a conjunction over bdds (nodes list) given. Takes global dag *)
	(* as input which holds each nodes from nodes list as bdd and returns the  *)
	(* global dag in modified form.                                            *)
	let conjunction (nList) (eList) (h) (t) =
		let rec conjunction' (nodesList) (expList) (m1) =
			Printf.printf "Recursion m = %d\n" m1;
			let rec cycleNodes m =
				Printf.printf "CycleNode m = %d\n" m;
				let element = (at m nodesList) in
				if (m1 < 2) then
					begin
						print_string
							("Not enough elements. Node only: \n"^(print_bdd element)^"\n");
						element
					end
				else if (m >1) then
					begin
						match element with
						| Zero -> print_string "Zero\n"; Zero
						| One -> print_string "One\n";
								conjunction' (drop nodesList m) (drop expList m) (m1 -1)
						| _ -> cycleNodes(m -1)
					end
				else
					begin
						match element with
						| Zero -> print_string "Zero\n"; Zero
						| One -> print_string "One\n";
								conjunction' (drop nodesList m) (drop expList m) (m1 -1)
						| _ -> let maxVar = select_max_var (nodesList) in
								let left =
									get_low_high_list_Left (nodesList) (expList) (h) (t) (maxVar) in
								let right =
									get_low_high_list_Right (nodesList) (expList) (h) (t) (maxVar) in
								let leftX = get_low_high_list_Vals_Left (expList) (maxVar) in
								let rightX = get_low_high_list_Vals_Right (expList) (maxVar) in
								let k1 = conjunction' (left) (leftX) (List.length left) in
								let k2 = conjunction' (right) (rightX) (List.length right) in
								make (maxVar) (k1) (k2) (h) (t);
					end
			in cycleNodes (m1)
		in conjunction' (nList) (eList) (List.length nList)
	
end;;