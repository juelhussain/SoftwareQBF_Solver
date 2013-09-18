


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
(*Output: val bdd_cn2 : bdd =
  Node (89,
   Node (90,
    Node (91, Node (93, Zero, Node (94, Node (95, Zero, One), One)),
     Node (94, Node (95, Zero, One), One)),
    Node (94, Node (95, Zero, One), One)),
   Node (90, Node (91, Node (93, Zero, One), One), One))*)
	
(* Simple tests: Take single variables as expression and *)
(* build conjunction between them*)
	
let h3 = Hashtbl.create 15;;
let t3 = Hashtbl.create 15;;
	
let exp1 = Var "1";;
let exp2 = Var "2";;
let exp3 = Var "3";;
let exp4 = Var "4";;
let exp5 = Var "5";;
let exp6 = Var "6";;
let exp7 = Var "7";;
let exp8 = Var "8";;
let exp9 = Var "9";;
let exp10 = Var "10";;
	
let bdd1 = build (exp1) (h3) (t3);;	
let bdd2 = build (exp2) (h3) (t3);;	
let bdd3 = build (exp3) (h3) (t3);;	
let bdd4 = build (exp4) (h3) (t3);;	
let bdd5 = build (exp5) (h3) (t3);;	
let bdd6 = build (exp6) (h3) (t3);;	
let bdd7 = build (exp7) (h3) (t3);;	
let bdd8 = build (exp8) (h3) (t3);;	
let bdd9 = build (exp9) (h3) (t3);;	
let bdd10 = build (exp10) (h3) (t3);;	

let bdd_cn10 = conjunction ([bdd1;bdd2;bdd3;bdd4;bdd5;bdd6;bdd7;bdd8;bdd9;bdd10]) 
([exp1;exp2;exp3;exp4;exp5;exp6;exp7;exp8;exp9;exp10]) (h3) (t3)

(*Output: val bdd_cn10 : bdd =
  Node (1, Zero,
   Node (2, Zero,
    Node (3, Zero,
     Node (4, Zero,
      Node (5, Zero,
       Node (6, Zero,
        Node (7, Zero, Node (8, Zero, Node (9, Zero, Node (10, Zero, One))))))))))*)

(*With empty lists: *)
conjunction ([])([])(h3)(t3);;
(*Output: Exception: Failure "List is empty".*)

(*With single node: *)
conjunction ([Node(1,Zero,One)])([Var "1"])(h3)(t3);;
(*Output:
Not enough elements. Node only: 
 Node ( 1, Zero , One  ) 
- : bdd = Node (1, Zero, One)*)

(*Conjunction with unstaisfiable clauses: *)
conjunction ([Node(1,Zero,One); Node(1,One,Zero)])([Var "1"; Neg(Var "1")])(h3)(t3);;
(*Output: Zero*)

(*Passing conjunction module without formula in CNF or DNF,*)
(* Data that can not be processed.*)

let h_bad = Hashtbl.create 15;;
let t_bad = Hashtbl.create 15;;

start_process([Imp(Var "3",And(True,False))]) (h_bad)(t_bad);;
(*Output: Exception: Failure "The clause is not in correct format".*)


start_process ([And(Var "1", Var "2"); Var "3"; 
Or(Var "1", Or (Var "3", Var "5")); Exists("1",True)]) (h_bad) (t_bad);;

start_process([Var "1";Var "2";Imp(Var "3",And(True,False))]) (h_bad)(t_bad);;
(*Adding node
Low:  Zero 
High:  One 
redundant test 2 - not adding node
Low:  Zero 
High:  One 
Exception: Failure "The clause is not in correct format".*)



(*e 1 2 3\\
1 3 4\\
2 5 7\\
1 -5 -2\\*)

let hashtable1 = Hashtbl.create 15;;
let hashtable2 = Hashtbl.create 15;;

let clauseList = ["e 1 2 3"; "1 3 4"; "2 5 7"; "1 -5 -2"]


let exp_list = convert_clauses_to_ExpressionList clauseList;;
(*Output: val exp_list : expression list =
  [Exists ("1 2 3", True); Or (Or (Var "1", Var "3"), Var "4");
   Or (Or (Var "2", Var "5"), Var "7");
   Or (Or (Var "1", Neg (Var "5")), Neg (Var "2"))]*)
	
print_exp_list (exp_list);;
(*Output: exists 
 1 \/  3 \/  4 
 2 \/  5 \/  7 
 1 \/  ~  5 \/  ~  2 
- : unit = ()*)

let bdd1 = build (List.nth exp_list 1) (hashtable1) (hashtable2)
(* Output: val bdd1 : bdd = Node (1, Node (3, Node (4, Zero, One), One), One) *)
let bdd2 = build (List.nth exp_list 2) (hashtable1) (hashtable2)
(* Output: val bdd2 : bdd = Node (2, Node (5, Node (7, Zero, One), One), One) *)
let bdd3 = build (List.nth exp_list 3) (hashtable1) (hashtable2)
(* Output: val bdd3 : bdd = Node (1, Node (2, One, Node (5, One, Zero)), One) *)


let bdd11 = build (eval(var_lookup (Var (string_of_int 1)) (List.nth exp_list 1) False)) (hashtable1) (hashtable2);;
(*Output: Node (3, Node (4, Zero, One), One)*)
let exp11 = eval(var_lookup (Var (string_of_int 1)) (List.nth exp_list 1) False)
(*Output: Or (Or (False, Var "3"), Var "4")*)		
																								
let bdd33 = build (eval(var_lookup (Var (string_of_int 1)) (List.nth exp_list 3) False)) (hashtable1) (hashtable2);;
(*Output: Node (2, One, Node (5, One, Zero))*)							
let exp33 = eval (var_lookup (Var (string_of_int 1)) (List.nth exp_list 3) False)
(* Or (Or (False, Neg (Var "5")), Neg (Var "2")) *)

	
let bdd1r = build (eval(var_lookup (Var (string_of_int 1)) (List.nth exp_list 1) True)) (hashtable1) (hashtable2);;
(*Output: One*)
let exp1r = eval (var_lookup (Var (string_of_int 1)) (List.nth exp_list 1) True)
(*Output: Or (Or (True, Var "3"), Var "4")*)																																																																																																										
																																																																																																																																																																																																																																																																																																																								
let bdd33r = build (eval(var_lookup (Var (string_of_int 1)) (List.nth exp_list 3) True)) (hashtable1) (hashtable2);;
(*Output: One*)
let exp33r = eval (var_lookup (Var (string_of_int 1)) (List.nth exp_list 3) True)
(* Or (Or (True, Neg (Var "5")), Neg (Var "2"))*)


(*The first recursive call to conjunction*)

let left_list_nodes = [bdd11;bdd2;bdd33];;
let right_list_nodes = [bdd1r; bdd2; bdd33r];;

let left_list_exp = [exp11;(List.nth exp_list 2);exp33];;
let right_list_exp = [exp1r;(List.nth exp_list 2);exp33r];;

conjunction (left_list_nodes) (left_list_exp) (hashtable1) (hashtable2)
(*Output: Node (2,
 Node (3, Node (4, Zero, Node (5, Node (7, Zero, One), One)),
  Node (5, Node (7, Zero, One), One)),
 Node (3, Node (4, Zero, Node (5, One, Zero)), Node (5, One, Zero)))*)

let lh_ht = Hashtbl.create 15;;

let max_var = select_max_var (left_list_nodes);;
let rec2 = get_low_high_list_Left (left_list_nodes) (left_list_exp) (hashtable1) (hashtable2) (max_var) (lh_ht)
(*Output: val rec2 : bdd list =
  [Node (3, Node (4, Zero, One), One); One;
   Node (5, Node (7, Zero, One), One)]*)
	
let rec2right = get_low_high_list_Right (right_list_nodes) (right_list_exp) (hashtable1) (hashtable2) (max_var) (lh_ht)
(*Output: val rec2right : bdd list = [One; One; One]*)

	

let bdd111 = build (eval(var_lookup (Var (string_of_int 2)) (List.nth left_list_exp 0) False)) (hashtable1) (hashtable2);;
(*Output: bdd =  bdd = Node (3, Node (4, Zero, One), One)*)
let exp111 = eval(var_lookup (Var (string_of_int 2)) (List.nth left_list_exp 0) False)
(*Output: expression = Or (Or (False, Var "3"), Var "4")*)		

let bdd222 = build (eval(var_lookup (Var (string_of_int 2)) (List.nth left_list_exp 1) False)) (hashtable1) (hashtable2);;
(*Output: bdd = Node (5, Node (7, Zero, One), One)*)
let exp222 = eval(var_lookup (Var (string_of_int 2)) (List.nth left_list_exp 1) False)
(*Output: expression = expression = Or (Or (False, Var "5"), Var "7")*)	

let bdd333 = build (eval(var_lookup (Var (string_of_int 2)) (List.nth left_list_exp 2) False)) (hashtable1) (hashtable2);;
(*Output: bdd = One*)
let exp333 = eval(var_lookup (Var (string_of_int 2)) (List.nth left_list_exp 2) False)
(*Output: expression = Or (Or (False, Neg (Var "5")), True)*)	



let rec2exp = get_low_high_list_Vals_Left (left_list_exp) (max_var) (Hashtbl.create 15)
(*Output: rec2exp : expression list =
  [Or (Or (False, Var "3"), Var "4"); Or (Or (False, Neg (Var "5")), True);
   Or (Or (False, Var "5"), Var "7")]*)
	
conjunction (rec2) (rec2exp) (hashtable1) (hashtable2)
		(*Output: Node (3, Node (4, Zero, Node (5, Node (7, Zero, One), One)),
 Node (5, Node (7, Zero, One), One))*)

	let max_var2 = select_max_var (rec2);;	(*Output 3*)		
	let rec3 = get_low_high_list_Left (rec2) (rec2exp) (hashtable1) (hashtable2) (max_var2) (Hashtbl.create 15)								
		(*Output: bdd list =
  [Node (4, Zero, One); Node (5, Node (7, Zero, One), One); One]*)	
	let rec3exp = get_low_high_list_Vals_Left (left_list_exp) (max_var) (Hashtbl.create 15)
	(*rec3exp : expression list =
  [Or (False, Var "4"); Or (Or (False, Var "5"), Var "7");
   Or (Or (False, Neg (Var "5")), True)]*)
	
	let rec3right = get_low_high_list_Right (rec2right) (rec3exp) (hashtable1) (hashtable2) (max_var2) (Hashtbl.create 15)
(*Output: val rec3right : bdd list = [One; One; One]*)
	
	conjunction (rec3) (rec3exp) (hashtable1) (hashtable2)
	
	
	let max_var3 = select_max_var (rec3);;		(*Output: 4*)	
	let rec4 = get_low_high_list_Left (rec3) (rec3exp) (hashtable1) (hashtable2) (max_var3) (Hashtbl.create 15);;								
		(*Output:bdd list = [Zero; One; Node (5, Node (7, Zero, One), One)]*)														
	let rec4exp = get_low_high_list_Vals_Left (rec3exp) (max_var3) (Hashtbl.create 15);;
		(*rec4exp : expression list =
  [False; Or (Or (False, Neg (Var "5")), True);
   Or (Or (False, Var "5"), Var "7")]*)
	
	let max_var4 = select_max_var (rec4);; (*Output: 5*)
	let rec5 = get_low_high_list_Left (rec4) (rec4exp) (hashtable1) (hashtable2) (max_var4) (Hashtbl.create 15)
	(*Output: bdd list = [Zero; Node (7, Zero, One); One]*)
	let rec5exp = get_low_high_list_Vals_Left (rec4exp) (max_var4) (Hashtbl.create 15);;
	(*[False; Or (False, Var "7"); Or (Or (False, True), True)]*)

	let max_var5 = select_max_var (rec5);; (*Output: 7*)
	let rec6 = get_low_high_list_Left (rec5) (rec5exp) (hashtable1) (hashtable2) (max_var5) (Hashtbl.create 15)
	(*Output: val rec6 : bdd list = [Zero; One; Zero]*)
	let rec6exp = get_low_high_list_Vals_Left (rec5exp) (max_var5) (Hashtbl.create 15);;
	(*Output: expression list = [False; Or (Or (False, True), True); False]*)
	
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