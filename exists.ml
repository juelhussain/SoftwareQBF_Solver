
module Exists =
struct
	
	let checkList listV var =
		let rec list1 i =
			if (i > (List.length listV) - 1) then
				false
			else
				begin
					if var = List.nth listV i then true
					else list1 (i +1)
				end
		in list1 (0)
	
	let checkListNo listV var =
		let rec list1 i =
			if (i > (List.length listV) - 1) then
				-1
			else
				begin
					if var = List.nth listV i then i +1
					else list1 (i +1)
				end
		in list1 (0)
	
	(* This gives a existential quantification over bdds (nodes list) and      *)
	(* quantified variables list given. Takes global dag as input which holds  *)
	(* each nodes from nodes list as bdd and returns the global dag in         *)
	(* modified form.                                                          *)
	let exists (vList) (nList) (eList) (h) (t) =
		let rec exists' (varList) (nodesList) (expList) (m1) =
			Printf.printf "Recursion m = %d\n" m1;
			let rec cycleNodes m =
				Printf.printf "CycleNode m = %d\n" m;
				if (m >1) then
					begin
						let element = (at m nodesList) in
						match element with
						| One -> print_string "One\n"; One
						| Zero -> print_string "Zero\n";
								exists' (varList) (drop nodesList m) (drop expList m) (m1 -1)
						| _ -> cycleNodes(m -1)
					end
				else if (m1 < 1) then
					begin
						print_string
							("0 elements\n");
						Zero
					end
				else
					begin
						let element = (at m nodesList) in
						match element with
						| One -> print_string "One\n"; One
						| Zero -> print_string "Zero\n";
								exists' (varList) (drop nodesList m) (drop expList m) (m1 -1)
						| _ -> let maxVar = select_max_var (nodesList) in
								let left =
									get_low_high_list_Left (nodesList) (expList) (h) (t) (maxVar) in
								let right =
									get_low_high_list_Right (nodesList) (expList) (h) (t) (maxVar) in
								let leftX = get_low_high_list_Vals_Left (expList) (maxVar) in
								let rightX = get_low_high_list_Vals_Right (expList) (maxVar) in
								if (checkList varList maxVar) then
									exists' (drop varList (checkListNo varList maxVar)) (left@right) (leftX@rightX)
										((List.length left) + (List.length right))
								else
									let k1 = exists' (varList) (left) (leftX) (List.length left) in
									let k2 = exists' (varList) (right) (rightX) (List.length right) in
									make (maxVar) (k1) (k2) (h) (t);
					end
			in cycleNodes (m1)
		in exists' (vList) (nList) (eList) (List.length nList)
	
end;;