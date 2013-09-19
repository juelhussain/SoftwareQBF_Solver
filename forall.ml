
module Forall =
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
	
	(* This gives a universal quantification over bdds (nodes list) given.   *)
	(* Takes global dag as input which holds each nodes from nodes list as   *)
	(* bdd and returns the global dag in modified form.                      *)
	let forall (vList) (nList) (eList) (h) (t) =
		let rec forall' (varList) (nodesList) (expList) (m1) =
			if (m1 < 1) then
					begin
						Zero
					end 
			else if((List.length nodesList) != (List.length expList)) then 
				raise (Failure "The expression list and nodes list does not match")
			else 
				begin
    			let rec cycleNodes m =
    				let element = (at m nodesList) in
    				if (m >1) then
    					begin
    						match element with
    						| Zero -> Zero
    						| One -> forall' (varList) (drop nodesList m) (drop expList m) (m1 -1)
    						| _ -> cycleNodes(m -1)
    					end
    				else
    					begin
    						match element with
    						| Zero -> Zero
    						| One -> forall' (varList) (drop nodesList m) (drop expList m) (m1 -1)
    						| _ -> 
    							begin
      							let maxVar = select_max_var (nodesList) in
    								let left =
    									get_left_bdd (nodesList) (expList) (maxVar) (h) (t) in
    								let right =
    									get_right_bdd (nodesList) (expList) (maxVar) (h) (t) in
    								let leftX = get_left_exp (expList) (maxVar) in
    								let rightX = get_right_exp (expList) (maxVar) in
										if (checkList varList maxVar) then
											(
												forall' (drop varList (checkListNo varList maxVar)) (left@right) (leftX@rightX)
												((List.length left) + (List.length right))
											)
										else
											(
    										let k1 = forall' (varList) (left) (leftX) (List.length left) in
    										let k2 = forall' (varList) (right) (rightX) (List.length right) in
    										make (maxVar) (k1) (k2) (h) (t)
											)
        					end
    					end
    			in cycleNodes (m1)
  			end
		in forall' (vList) (nList) (eList) (List.length nList)
	
end;;
