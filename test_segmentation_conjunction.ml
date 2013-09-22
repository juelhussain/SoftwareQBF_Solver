

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
	
let time f x y z a =
    let t = Sys.time() in
    let fxyza = f x y z a in
    Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
    fxyza
	
	
Printf.printf "Testing: %s" (get_print_bdd_list (con_list))

let con_list2 = start_process_segmentation (expList) (hseg) (tseg) (2);;


let time2 f x y z =
    let t = Sys.time() in
    let fxyz = f x y z in
    Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
    fxyz
		
start_process (expList) (hseg) (tseg)
(* Execution time: 0.000773s
- : bdd =
Node (1, Zero,
 Node (2, Zero, Node (3, Zero, Node (4, Zero, Node (5, Zero, One)))))*)

let time f w x y z =
    let t = Sys.time() in
    let fwxyz = f w x y z in
    Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
    fwxyz


time (start_process_segmentation) (expList) (hseg) (tseg) (5)

let expList2 = [Var "1";Var "2";Var "3";Var "4";Var "5";And(Var "3",Or(Var "4", And (Var "5", Var "6")));Or(Var "7",And(Var "8",Var "9"))]

let exec_time = time (start_process_segmentation) (expList) (hseg) (tseg) (2);;
(* Execution time: 0.000622s
val exec_time : bdd list =
  [Node (1, Zero, Node (2, Zero, One)); Node (3, Zero, Node (4, Zero, One));
   Node (5, Zero, One)] *)
let exec_time = time (start_process_segmentation) (expList2) (hseg) (tseg) (2);;
(*Execution time: 0.002242s
val exec_time : bdd list =
  [Node (1, Zero, Node (2, Zero, One)); Node (3, Zero, Node (4, Zero, One));
   Node (3, Zero,
    Node (4, Node (5, Zero, Node (6, Zero, One)), Node (5, Zero, One)));
   Node (7, Node (8, Zero, Node (9, Zero, One)), One)]*)

time2 start_process (expList2) (hseg) (tseg)
(* 0.001953s
- : bdd =
Node (1, Zero,
 Node (2, Zero,
  Node (3, Zero,
   Node (4, Zero,
    Node (5, Zero, Node (7, Node (8, Zero, Node (9, Zero, One)), One))))))*)

	

let expList3 = [Var "1";Var "2";Var "3";Var "4";Var "5";And(Var "3",Or(Var "4", And (Var "5", Var "6")));Or(Var "7",And(Var "8",Var "9"));
And(Var "8",Or(Var "1", And (Var "9", Var "4")));Or(Var "2",And(Var "7",Var "9"));
And(Var "3",Or(Var "2", And (Var "4", Var "3")));Or(Var "7",And(Var "9",Var "1"));
And(Var "1",Or(Var "4", And (Var "5", Var "6")));Or(Var "2",And(Var "8",Var "1"))]
		
let exec_time3 = time (start_process_segmentation) (expList3) (hseg) (tseg) (2);;	
(* Execution time: 0.008558s
val exec_time3 : bdd list =
  [Node (1, Zero, Node (2, Zero, One)); Node (3, Zero, Node (4, Zero, One));
   Node (3, Zero,
    Node (4, Node (5, Zero, Node (6, Zero, One)), Node (5, Zero, One)));
   Node (1,
    Node (4, Zero,
     Node (7, Node (8, Zero, Node (9, Zero, One)),
      Node (8, Zero, Node (9, Zero, One)))),
    Node (4,
     Node (7, Node (8, Zero, Node (9, Zero, One)), Node (8, Zero, One)),
     Node (7, Node (8, Zero, Node (9, Zero, One)), Node (8, Zero, One))));
   Node (2,
    Node (3, Zero, Node (4, Zero, Node (7, Zero, Node (9, Zero, One)))),
    Node (3, Zero, One));
   Node (1, Zero,
    Node (4,
     Node (5, Zero, Node (6, Zero, Node (7, Node (9, Zero, One), One))),
     Node (7, Node (9, Zero, One), One)));
   Node (1, Node (2, Zero, One), Node (2, Node (8, Zero, One), One))]*)
	
let exec_time31 = time2 (start_process) (expList3) (hseg) (tseg);;		
(*0.006546s
val exec_time31 : bdd =
  Node (1, Zero,
   Node (2, Zero,
    Node (3, Zero,
     Node (4, Zero,
      Node (5, Zero,
       Node (7, Node (8, Zero, Node (9, Zero, One)), Node (8, Zero, One)))))))*)					
						
							
let exec_time4 = time (start_process_segmentation) (expList3) (hseg) (tseg) (4);;	
(*  Execution time: 0.010517s
val exec_time4 : bdd list =
  [Node (1, Zero, Node (2, Zero, Node (3, Zero, Node (4, Zero, One))));
   Node (1,
    Node (3, Zero,
     Node (4, Zero,
      Node (5, Zero,
       Node (7, Node (8, Zero, Node (9, Zero, One)),
        Node (8, Zero, Node (9, Zero, One)))))),
    Node (3, Zero,
     Node (4,
      Node (5, Zero,
       Node (6, Zero,
        Node (7, Node (8, Zero, Node (9, Zero, One)), Node (8, Zero, One)))),
      Node (5, Zero,
       Node (7, Node (8, Zero, Node (9, Zero, One)), Node (8, Zero, One))))));
   Node (1, Zero,
    Node (2,
     Node (3, Zero, Node (4, Zero, Node (7, Zero, Node (9, Zero, One)))),
     Node (3, Zero,
      Node (4,
       Node (5, Zero, Node (6, Zero, Node (7, Node (9, Zero, One), One))),
       Node (7, Node (9, Zero, One), One)))));
   Node (1, Node (2, Zero, One), Node (2, Node (8, Zero, One), One))]*)	
										
let expList4 = [Var "1";Var "2";Var "3";Var "4";Var "5";And(Var "6",Or(Var "7", And (Var "6", Var "8")));Or(Var "9",And(Var "10",Var "11"));
And(Var "12",Or(Var "13", And (Var "12", Var "14")));Or(Var "12",And(Var "17",Var "19"));
And(Var "13",Or(Var "12", And (Var "14", Var "13")));Or(Var "17",And(Var "19",Var "12"));
And(Var "19",Or(Var "14", And (Var "15", Var "16")));Or(Var "12",And(Var "18",Var "11"))]
												
let exec_time4 = time (start_process_segmentation) (expList4) (hseg) (tseg) (4);;	
(*	Execution time: 0.012511s
val exec_time4 : bdd list =
  [Node (1, Zero, Node (2, Zero, Node (3, Zero, Node (4, Zero, One))));
   Node (5, Zero,
    Node (6, Zero,
     Node (7,
      Node (8, Zero,
       Node (9,
        Node (10, Zero,
         Node (11, Zero,
          Node (12, Zero, Node (13, Node (14, Zero, One), One)))),
        Node (12, Zero, Node (13, Node (14, Zero, One), One)))),
      Node (9,
       Node (10, Zero,
        Node (11, Zero,
         Node (12, Zero, Node (13, Node (14, Zero, One), One)))),
       Node (12, Zero, Node (13, Node (14, Zero, One), One))))));
   Node (12,
    Node (13, Zero,
     Node (14, Zero,
      Node (15,
       Node (16, Node (17, Zero, Node (19, Zero, One)),
        Node (17, Zero, Node (19, Zero, One))),
       Node (16, Node (17, Zero, Node (19, Zero, One)),
        Node (17, Zero, Node (19, Zero, One)))))),
    Node (13, Zero,
     Node (14,
      Node (15, Zero,
       Node (16, Zero, Node (17, Node (19, Zero, One), Node (19, Zero, One)))),
      Node (15,
       Node (16, Node (17, Node (19, Zero, One), Node (19, Zero, One)),
        Node (17, Node (19, Zero, One), Node (19, Zero, One))),
       Node (16, Node (17, Node (19, Zero, One), Node (19, Zero, One)),
        Node (17, Node (19, Zero, One), Node (19, Zero, One)))))));
   Node (11, Node (12, Zero, One), Node (12, Node (18, Zero, One), One))]*)														
															
let exec_time41 = time2 (start_process) (expList4) (hseg) (tseg);;					
		(* Execution time: 0.034949s
val exec_time41 : bdd =
  Node (1, Zero,
   Node (2, Zero,
    Node (3, Zero,
     Node (4, Zero,
      Node (5, Zero,
       Node (6, Zero,
        Node (7,
         Node (8, Zero,
          Node (9,
           Node (10, Zero,
            Node (11, Zero,
             Node (12, Zero,
              Node (13, Zero,
               Node (14,
                Node (15, Zero,
                 Node (16, Zero,
                  Node (17, Node (19, Zero, One), Node (19, Zero, One)))),
                Node (15,
                 Node (16,
                  Node (17, Node (19, Zero, One), Node (19, Zero, One)),
                  Node (17, Node (19, Zero, One), Node (19, Zero, One))),
                 Node (16,
                  Node (17, Node (19, Zero, One), Node (19, Zero, One)),
                  Node (17, Node (19, Zero, One), Node (19, Zero, One))))))))),
           Node (11,
            Node (12, Zero,
             Node (13, Zero,
              Node (14,
               Node (15, Zero,
                Node (16, Zero,
                 Node (17, Node (19, Zero, One), Node (19, Zero, One)))),
               Node (15,
                Node (16,
                 Node (17, Node (19, Zero, One), Node (19, Zero, One)),
                 Node (17, Node (19, Zero, One), Node (19, Zero, One))),
                Node (16,
                 Node (17, Node (19, Zero, One), Node (19, Zero, One)),
                 Node (17, Node (19, Zero, One), Node (19, Zero, One))))))),
            Node (12, Zero,
             Node (13, Zero,
              Node (14,
               Node (15, Zero,
                Node (16, Zero,
                 Node (17, Node (19, Zero, One), Node (19, Zero, One)))),
               Node (15,
                Node (16,
                 Node (17, Node (19, Zero, One), Node (19, Zero, One)),
                 Node (17, Node (19, Zero, One), Node (19, Zero, One))),
                Node (16,
                 Node (17, Node (19, Zero, One), Node (19, Zero, One)),
                 Node (17, Node (19, Zero, One), Node (19, Zero, One)))))))))),
         Node (9,
          Node (10, Zero,
           Node (11, Zero,
            Node (12, Zero,
             Node (13, Zero,
              Node (14,
               Node (15, Zero,
                Node (16, Zero,
                 Node (17, Node (19, Zero, One), Node (19, Zero, One)))),
               Node (15,
                Node (16,
                 Node (17, Node (19, Zero, One), Node (19, Zero, One)),
                 Node (17, Node (19, Zero, One), Node (19, Zero, One))),
                Node (...))))))),
          ...)))))))) *)

print_bdd_list (exec_time4);;
 (*Node ( 1, Zero , Node ( 2, Zero , Node ( 3, Zero , Node ( 4, Zero , One  )  )  )  )  
 Node ( 5, Zero , Node ( 6, Zero , Node ( 7, Node ( 8, Zero , Node ( 9, Node ( 10, Zero , 
Node ( 11, Zero , Node ( 12, Zero , Node ( 13, Node ( 14, Zero , One  ) , One  )  )  )  ) , 
Node ( 12, Zero , Node ( 13, Node ( 14, Zero , One  ) , One  )  )  )  ) , Node ( 9, 
Node ( 10, Zero , Node ( 11, Zero , Node ( 12, Zero , Node ( 13, 
Node ( 14, Zero , One  ) , One  )  )  )  ) , Node ( 12, Zero , Node ( 13, 
Node ( 14, Zero , One  ) , One  )  )  )  )  )  )  
 Node ( 12, Node ( 13, Zero , Node ( 14, Zero , Node ( 15, Node ( 16, Node ( 17, Zero , 
Node ( 19, Zero , One  )  ) , Node ( 17, Zero , Node ( 19, Zero , One  )  )  ) , 
Node ( 16, Node ( 17, Zero , Node ( 19, Zero , One  )  ) , Node ( 17, Zero , 
Node ( 19, Zero , One  )  )  )  )  )  ) , Node ( 13, Zero , Node ( 14, Node ( 15, Zero , 
Node ( 16, Zero , Node ( 17, Node ( 19, Zero , One  ) , Node ( 19, Zero , One  )  )  )  ) , 
Node ( 15, Node ( 16, Node ( 17, Node ( 19, Zero , One  ) , Node ( 19, Zero , One  )  ) , 
Node ( 17, Node ( 19, Zero , One  ) , Node ( 19, Zero , One  )  )  ) , Node ( 16, Node ( 17, 
Node ( 19, Zero , One  ) , Node ( 19, Zero , One  )  ) , Node ( 17, Node ( 19, Zero , One  ) , 
Node ( 19, Zero , One  )  )  )  )  )  )  )  
 Node ( 11, Node ( 12, Zero , One  ) , Node ( 12, Node ( 18, Zero , One  ) , One  )  )  
- : unit = ()
*)

print_bdd (exec_time41);;
 (*Node ( 1, Zero , Node ( 2, Zero , Node ( 3, Zero , Node ( 4, Zero , *)
(* Node ( 5, Zero , Node ( 6, Zero , Node ( 7, Node ( 8, Zero , Node ( 9, *)
(* Node ( 10, Zero , Node ( 11, Zero , Node ( 12, Zero , Node ( 13, Zero , *)
(* Node ( 14, Node ( 15, Zero , Node ( 16, Zero , Node ( 17, Node ( 19, Zero , One  ) , *)
(* Node ( 19, Zero , One  )  )  )  ) , Node ( 15, Node ( 16, Node ( 17, *)
(* Node ( 19, Zero , One  ) , Node ( 19, Zero , One  )  ) , Node ( 17, *)
(* Node ( 19, Zero , One  ) , Node ( 19, Zero , One  )  )  ) , Node ( 16, *)
(* Node ( 17, Node ( 19, Zero , One  ) , Node ( 19, Zero , One  )  ) , Node ( 17, *)
(* Node ( 19, Zero , One  ) , Node ( 19, Zero , One  )  )  )  )  )  )  )  )  ) , *)
(* Node ( 11, Node ( 12, Zero , Node ( 13, Zero , Node ( 14, Node ( 15, Zero , *)
(* Node ( 16, Zero , Node ( 17, Node ( 19, Zero , One  ) , *)
(* Node ( 19, Zero , One  )  )  )  ) , Node ( 15, Node ( 16, Node ( 17, *)
(* Node ( 19, Zero , One  ) , Node ( 19, Zero , One  )  ) , Node ( 17, *)
(* Node ( 19, Zero , One  ) , Node ( 19, Zero , One  )  )  ) , Node ( 16, *)
(* Node ( 17, Node ( 19, Zero , One  ) , Node ( 19, Zero , One  )  ) , *)
(* Node ( 17, Node ( 19, Zero , One  ) , Node ( 19, Zero , One  )  )  )  )  )  )  ) , *)
(* Node ( 12, Zero , Node ( 13, Zero , Node ( 14, Node ( 15, Zero , *)
(* Node ( 16, Zero , Node ( 17, Node ( 19, Zero , One  ) , *)
(* Node ( 19, Zero , One  )  )  )  ) , Node ( 15, Node ( 16, Node ( 17, *)
(* Node ( 19, Zero , One  ) , Node ( 19, Zero , One  )  ) , Node ( 17, *)
(* Node ( 19, Zero , One  ) , Node ( 19, Zero , One  )  )  ) , Node ( 16, *)
(* Node ( 17, Node ( 19, Zero , One  ) , Node ( 19, Zero , One  )  ) , *)
(* Node ( 17, Node ( 19, Zero , One  ) , Node ( 19, Zero , One  )  )  )  )  )  )  )  )  )  ) , *)
(* Node ( 9, Node ( 10, Zero , Node ( 11, Zero , Node ( 12, Zero , Node ( 13, Zero , *)
(* Node ( 14, Node ( 15, Zero , Node ( 16, Zero , Node ( 17, Node ( 19, Zero , One  ) , *)
(* Node ( 19, Zero , One  )  )  )  ) , Node ( 15, Node ( 16, Node ( 17, *)
(* Node ( 19, Zero , One  ) , Node ( 19, Zero , One  )  ) , Node ( 17, *)
(* Node ( 19, Zero , One  ) , Node ( 19, Zero , One  )  )  ) , Node ( 16, Node ( 17, *)
(* Node ( 19, Zero , One  ) , Node ( 19, Zero , One  )  ) , Node ( 17, *)
(* Node ( 19, Zero , One  ) , Node ( 19, Zero , One  )  )  )  )  )  )  )  )  ) , *)
(* Node ( 11, Node ( 12, Zero , Node ( 13, Zero , Node ( 14, Node ( 15, Zero , *)
(* Node ( 16, Zero , Node ( 17, Node ( 19, Zero , One  ) , Node ( 19, Zero , One  )  )  )  ) , *)
(* Node ( 15, Node ( 16, Node ( 17, Node ( 19, Zero , One  ) , Node ( 19, Zero , One  )  ) , *)
(* Node ( 17, Node ( 19, Zero , One  ) , Node ( 19, Zero , One  )  )  ) , Node ( 16, *)
(* Node ( 17, Node ( 19, Zero , One  ) , Node ( 19, Zero , One  )  ) , Node ( 17, *)
(* Node ( 19, Zero , One  ) , Node ( 19, Zero , One  )  )  )  )  )  )  ) , *)
(* Node ( 12, Zero , Node ( 13, Zero , Node ( 14, Node ( 15, Zero , Node ( 16, Zero , *)
(* Node ( 17, Node ( 19, Zero , One  ) , Node ( 19, Zero , One  )  )  )  ) , *)
(* Node ( 15, Node ( 16, Node ( 17, Node ( 19, Zero , One  ) , Node ( 19, Zero , One  )  ) , *)
(* Node ( 17, Node ( 19, Zero , One  ) , Node ( 19, Zero , One  )  )  ) , Node ( 16, *)
(* Node ( 17, Node ( 19, Zero , One  ) , Node ( 19, Zero , One  )  ) , Node ( 17, *)
(* Node ( 19, Zero , One  ) , Node ( 19, Zero , One  )  )  )  )  )  )  )  )  )  )  )  )  )  )  )  ) - : unit = ()








