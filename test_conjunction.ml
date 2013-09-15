


(* This will be a script to test Conjunction *)





let h_testC = createHT 15;;
let t_testC = createHT 15;;

build (And(Var "3", Var "2")) (h_testC) (t_testC);;
Hashtbl.find h_testC (Node (2, Zero, Node (3, Zero, One)));;

build (And(Var "2", Var "4")) (h_testC) (t_testC);;
Hashtbl.find h_testC (Node (2, Zero, Node (4, Zero, One)));;

conjunction ([Node (2, Zero, Node (3, Zero, One));Node (2, Zero, Node (4, Zero, One))])
([And(Var "3", Var "2");And(Var "2", Var "4")]) (h_testC) (t_testC);;



let h_test_c = Hashtbl.create 15;;
let t_test_c = Hashtbl.create 15;;

let exp1 = Or(Or(Var "1", Or(Var "2", Var "4")),Var "3");;
let exp2 = Or(Var "3",Or(Var "4", Neg(Var "2")));;
let exp3 = And(Var "1", Or(Var "3", And(Var "2", Var "1")));;

let bdd1 = build(exp1) (h_test_c) (t_test_c);;
	(* Output:  Node (1, Node (2, Node (3, Node (4, Zero, One), One), One), One)*)
let bdd2 = build(exp2) (h_test_c) (t_test_c);;
	(* Output: Node (2, One, Node (3, Node (4, Zero, One), One)) *)
let bdd3 = build(exp3) (h_test_c) (t_test_c);;
	(* Output: Node (1, Zero, Node (2, Node (3, Zero, One), One)) *)

let bdd_cn = conjunction ([bdd1;bdd2;bdd3]) ([exp1;exp2;exp3]) (h_test_c) (t_test_c);;
	(* Output: Node (1, Zero,
   Node (2, Node (3, Zero, One), Node (3, Node (4, Zero, One), One))) *)

let bdd_dn = disjunction ([bdd1;bdd2;bdd3]) ([exp1;exp2;exp3]) (h_test_c) (t_test_c);;
	(* Output: One *)


(* Running the start module with the above formulas we get the same result*)
let h_c = Hashtbl.create 15;;
let t_c = Hashtbl.create 15;;
start_process ([exp1;exp2;exp3]) (h_c) (t_c);;
	(* Output: Node (1, Zero,
 Node (2, Node (3, Zero, One), Node (3, Node (4, Zero, One), One))) *)

let h2 = Hashtbl.create 15;;
let t2 = Hashtbl.create 15;;
let exp2 = (Or(Or(Var "95",Var "89"), Var "94"));;
let exp22 = (Or(Or(Var "91",Var "93"), Var "90"));;

let bdd2 = build (exp2) (h2) (t2);;
let bdd22 = build (exp22) (h2) (t2);;

let bdd_cn2 = conjunction ([bdd2;bdd22]) ([exp2;exp22]) (h2) (t2)


(* This gives a conjunction over bdds (nodes list) given. Takes global dag *)
	(* as input which holds each nodes from nodes list as bdd and returns the  *)
	(* global dag in modified form.                                            *)
	let conjunction (nList) (eList) (h) (t) =
		let rec conjunction' (nodesList) (expList) (m1) =
			let rec cycleNodes m =
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
						| Zero -> Zero
						| One -> conjunction' (drop nodesList m) (drop expList m) (m1 -1)
						| _ -> cycleNodes(m -1)
					end
				else
					begin
						match element with
						| Zero -> Zero
						| One -> conjunction' (drop nodesList m) (drop expList m) (m1 -1)
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