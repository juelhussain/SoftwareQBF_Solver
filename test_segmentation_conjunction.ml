

let expList = [Var "1";Var "2";Var "3";Var "4";Var "5"]
let nodesList = [Node(1,Zero,One);Node(2,Zero,One);Node(3,Zero,One);Node(4,Zero,One);Node(5,Zero,One)]
	
let hseg = Hashtbl.create 15
let tseg = Hashtbl.create 15;;
	
let test_c = segment_conjunction ([Var "1";Var "2";Var "3";Var "4";Var "5"]) 
([Node(1,Zero,One);Node(2,Zero,One);Node(3,Zero,One);Node(4,Zero,One);Node(5,Zero,One)]) 
(h1) (t1) (2);;
(*bdd list =
[Node (1, Zero, Node (2, Zero, One)); Node (3, Zero, Node (4, Zero, One));
 Node (5, Zero, One)]*)

let c1 = conjunction ([Node(1,Zero,One);Node(2,Zero,One);Node(3,Zero,One);
Node(4,Zero,One);Node(5,Zero,One)]) 
([Var "1";Var "2";Var "3";Var "4";Var "5"]) (h1) (t1);;
(*Output: bdd =
Node (1, Zero,
 Node (2, Zero, Node (3, Zero, Node (4, Zero, Node (5, Zero, One)))))*)


let c2 = conjunction ([Node (1, Zero, Node (2, Zero, One));
Node (3, Zero, Node (4, Zero, One));
Node (5, Zero, One)]) 
([And(Var "1",Var "2");And(Var "3",Var "4");Var "5"]) (h1) (t1);;
(*Output: bdd =
Node (1, Zero,
 Node (2, Zero, Node (3, Zero, Node (4, Zero, Node (5, Zero, One)))))*)

c1=c2;;
(*Output: bool = true*)



let con_list = start_process_segmentation_old (expList) (hseg) (tseg) (2);;
(*Output: bdd list =
  [Node (1, Zero, Node (2, Zero, One)); Node (3, Zero, Node (4, Zero, One));
   Node (5, Zero, One)]*)
	
let con_list2 = start_process_segmentation (expList) (hseg) (tseg) (2);;
(*bdd list =
  [Node (1, Zero, Node (2, Zero, One)); Node (3, Zero, Node (4, Zero, One));
   Node (5, Zero, One)]*)
	
Printf.printf "Testing: %s" (get_print_bdd_list (con_list))

let con_list2 = start_process_segmentation (expList) (hseg) (tseg) (2);;

let func x = x*2 


time (start_process_segmentation) (expList) (hseg) (tseg) (2)

start_process (exp_list) (hseg) (tseg)

