
let string_list1 = "1 2 3 4 5"
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
	
let con_list3 = start_process_segmentation_double (["1";"2";"3";"4";"5"]) (hseg) (tseg) (2) (5);;	
(*val con_list3 : bdd list =
  [Node (1, Zero, Node (2, Zero, One)); Node (3, Zero, Node (4, Zero, One));
   Node (5, Zero, One)]*)

let time f x y z a =
    let t = Sys.time() in
    let fxyza = f x y z a in
    Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
    fxyza;;

let time2 f x y z a b=
    let t = Sys.time() in
    let fxyzab = f x y z a b in
    Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
    fxyzab
	
	
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

*)

let testStringExp = ["38 36 29 34 35 41 46 50 44 40 27 274 49 42 31 43 33 30 32 39 51 48 28 37 47 45 0";
"357 -130 0"];;
let clauses = convert_clauses_to_ExpressionList (testStringExp);;

let testh = Hashtbl.create 15;;
let testt = Hashtbl.create 15;;

let node1 = build (List.nth clauses 0) (testh) (testt)
let node2 = build (List.nth clauses 1) (testh) (testt)



(*Comparing process segmentation vs segmentation-double *)

let time f x y z a =
    let t = Sys.time() in
    let fxyza = f x y z a in
    Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
    fxyza;;

let time2 f x y z a b=
    let t = Sys.time() in
    let fxyzab = f x y z a b in
    Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
    fxyzab
	

let seg_h = Hashtbl.create 15;;
let seg_t = Hashtbl.create 15;;
let string_clause_list = ["1 2 3 4 5 6 7 8"; "9 10 11 12 13 14";
"15 16 17 18 19 20 21 22"; "23 24 25";
"15 26 67 23 9 90 23 13 11"; "90 93 84 55 48 39 83 49 54 45"]

let exp_clauses = convert_clauses_to_ExpressionList string_clause_list
let time1 = time (start_process_segmentation) (exp_clauses) (seg_h) (seg_t) (2);;	
(*Execution time: 0.475646s*)
let time1 = time (start_process_segmentation) (exp_clauses) (seg_h) (seg_t) (4);;
(*Execution time: 0.752461s*)

let seg_h2 = Hashtbl.create 15;;
let seg_t2 = Hashtbl.create 15;;
let time1 = time2 (start_process_segmentation_double) (string_clause_list) (seg_h2) (seg_t2) (2) (4);;	
(*Execution time: 0.442621s*)

let time1 = time2 (start_process_segmentation_double) (string_clause_list) (seg_h2) (seg_t2) (4) (4);;
(*Execution time: 0.714224s*)

let string_clause_list2 = ["1 2 3 4 5 6 7 8"; "9 10 11 12 13 14 15 16 17 18 19 20 21 22"; 
"23 24 25 15 26 67 23 9 90 23 13 11"; "90 93 84 55 48 39 83 49 54 45"]
let seg_h3 = Hashtbl.create 15;;
let seg_t3 = Hashtbl.create 15;;
let exp_clauses2 = convert_clauses_to_ExpressionList string_clause_list2
let time1 = time (start_process_segmentation) (exp_clauses2) (seg_h3) (seg_t3) (2);;	
(*Execution time: 1.095329s*)

let time2 = time (start_process_segmentation) (exp_clauses2) (seg_h3) (seg_t3) (4);;	
(*Execution time: 20.327943s*)

let seg_h4 = Hashtbl.create 15;;
let seg_t4 = Hashtbl.create 15;;
let time3 =  time2 (start_process_segmentation_double) (string_clause_list2) (seg_h4) (seg_t4) (2) (4);;
(*Execution time: 0.875706s*)

let time4 =  time2 (start_process_segmentation_double) (string_clause_list2) (seg_h4) (seg_t4) (4) (4);;
(*Execution time: 19.922851s*)

start_process_segmentation_double (["e 1 2 4"]@string_clause_list2) (seg_h4) (seg_t4) (4) (4);;

let string_clause_list3 = ["1 2 3 4 5 6 7 8 2 32 12 32 41 1 23 31 21 1"; "9 10 11 12 13 14 15 16 17 18 19 20 21 22"; 
"23 24 25 15 26 67 23 9 90 23 13 11 12 21 12 21 1 12 9"; "90 93 84 55 48 39 83 49 54 45 23 32 26 62 11 23";
"10 13 12 1 12 12 32 31"]

let seg_h5 = Hashtbl.create 15;;
let seg_t5 = Hashtbl.create 15;;
let exp_clauses3 = convert_clauses_to_ExpressionList string_clause_list3
let time_3 = time (start_process_segmentation) (exp_clauses3) (seg_h5) (seg_t5) (2);;	
(*Execution time: 33.018370s*)

let time_4 = time (start_process_segmentation) (exp_clauses3) (seg_h5) (seg_t5) (4);;	


let seg_h6 = Hashtbl.create 15;;
let seg_t6 = Hashtbl.create 15;;
let time5 =  time2 (start_process_segmentation_double) (string_clause_list3) (seg_h6) (seg_t6) (2) (4);;
(*Execution time: 25.060377s*)

let time4 =  time2 (start_process_segmentation_double) (string_clause_list2) (seg_h4) (seg_t4) (4) (4);;
(*Execution time: 19.922851s*)


(*NEW Segmentation with variable order test*)
let hord_1 = Hashtbl.create 15;;
let tord_1 = Hashtbl.create 15;;

let hord_2 = Hashtbl.create 15;;
let tord_2 = Hashtbl.create 15;;
		
let clauseList_1 = ["1 2 -3"; "4 5 -7"]
let clauseList_2 = ["3 -2 1"; "7 -5 3"]
let exp_list_1 = convert_clauses_to_ExpressionList clauseList_1;; 
let exp_list_2 = convert_clauses_to_ExpressionList clauseList_2;;

let one = start_process (exp_list_1)(hord_1)(tord_1)
(*Output:  Node (1,
   Node (2,
    Node (3, Node (4, Node (5, Node (7, One, Zero), One), One), Zero),
    Node (4, Node (5, Node (7, One, Zero), One), One)),
   Node (4, Node (5, Node (7, One, Zero), One), One))*)
	
let two = start_process (exp_list_2)(hord_2)(tord_2)
(*Output: Node (1,
   Node (2, Node (3, Node (5, One, Node (7, Zero, One)), One),
    Node (3, Zero, One)),
   Node (3, Node (5, One, Node (7, Zero, One)), One))*)
	
	
let conjunction_1 = start_process_segmentation (exp_list_1@exp_list_2) (Hashtbl.create 15) (Hashtbl.create 15) (2);;
 (*Output: [Node (1, Node (2, Node (3, One, Zero), Node (3, Zero, One)), One);
   Node (3,
    Node (4, Node (5, Node (7, One, Zero), Node (7, Zero, One)),
     Node (5, One, Node (7, Zero, One))),
    Node (4, Node (5, Node (7, One, Zero), One), One))]*)

let inf_1 = to_cnf_list(translate_list_to_cnf(conjunction_1));;

print_string (get_exp_list_to_qdimacs(inf_1));;
(* 
-7 5 4 -1 0
-7 5 4 -2 1 0
-3 2 1 0
-7 5 4 3 2 1 0
7 -5 3 -1 0
3 -2 1 0
7 -5 3 2 1 0*)
	
	
let hord_3 = Hashtbl.create 15;;
let tord_3 = Hashtbl.create 15;;

let hord_4 = Hashtbl.create 15;;
let tord_4 = Hashtbl.create 15;;
		
let clauseList_3 = ["1 2 -3";"3 -2 1"]
let clauseList_4 = ["4 5 -7";"7 -5 3"]
let exp_list_3 = convert_clauses_to_ExpressionList clauseList_3;; 
let exp_list_4 = convert_clauses_to_ExpressionList clauseList_4;;

let three = start_process (exp_list_3)(hord_3)(tord_3)
(*Output:   Node (1, Node (2, Node (3, One, Zero), Node (3, Zero, One)), One)*)
	
let four = start_process (exp_list_4)(hord_4)(tord_4)
(*Output: Node (3,
   Node (4, Node (5, Node (7, One, Zero), Node (7, Zero, One)),
    Node (5, One, Node (7, Zero, One))),
   Node (4, Node (5, Node (7, One, Zero), One), One))*)
	
let conjunction3 = start_process_segmentation (exp_list_3@exp_list_4) (Hashtbl.create 15) (Hashtbl.create 15) (2);;
 (*Output: [Node (1, Node (2, Node (3, One, Zero), Node (3, Zero, One)), One);
   Node (3,
    Node (4, Node (5, Node (7, One, Zero), Node (7, Zero, One)),
     Node (5, One, Node (7, Zero, One))),
    Node (4, Node (5, Node (7, One, Zero), One), One))]*)

let inf = to_cnf_list(translate_list_to_cnf(conjunction3));;

print_string (get_exp_list_to_qdimacs(inf));;
(* Output: 
3 -2 1 0
-3 2 1 0
-7 5 4 -3 0
7 -5 -4 3 0
7 -5 4 3 0
-7 5 4 3 0
*)

(*The difference between ordering and non-ordering: *)
(* Non order:
-7 5 4 -1 0
-7 5 4 -2 1 0
-3 2 1 0
-7 5 4 3 2 1 0
7 -5 3 -1 0
3 -2 1 0
7 -5 3 2 1 0
VS
Order:
3 -2 1 0
-3 2 1 0
-7 5 4 -3 0
7 -5 -4 3 0
7 -5 4 3 0
-7 5 4 3 0
*)


let hord = Hashtbl.create 15;;
let tord = Hashtbl.create 15;;
		
let clauseList = ["1 2 -3"; "4 5 -7"; "3 -2 1"; "7 -5 3"]
let exp_list = convert_clauses_to_ExpressionList clauseList;;

let conjunction1 = start_process_segmentation (exp_list) (hord) (tord) (2)
(*Output: [Node (1,Node (2,Node (3, Node (4, Node (5, Node (7, One, Zero), One), One), Zero),
     Node (4, Node (5, Node (7, One, Zero), One), One)),Node (4, Node (5, Node (7, One, Zero), One), One));
   Node (1,Node (2, Node (3, Node (5, One, Node (7, Zero, One)), One),
     Node (3, Zero, One)),Node (3, Node (5, One, Node (7, Zero, One)), One))]*)

let inf1 = to_cnf_list(translate_list_to_cnf(conjunction1));;
print_string (get_exp_list_to_qdimacs(inf1));;
(*
-7 5 4 -1 0
-7 5 4 -2 1 0
-3 2 1 0
-7 5 4 3 2 1 0
7 -5 3 -1 0
3 -2 1 0
7 -5 3 2 1 0
*)

let hord2 = Hashtbl.create 15;;
let tord2 = Hashtbl.create 15;;

let clauseList2 = ["4 5 -7"; "7 -5 3";"3 -2 1";"1 2 -3"]
let exp_list2 = convert_clauses_to_ExpressionList clauseList2;;

let conjunction2 = start_process_segmentation (exp_list2) (hord2) (tord2) (2)

let inf2 = to_cnf_list(translate_list_to_cnf(conjunction2));;
print_string (get_exp_list_to_qdimacs(inf2));;
(**)
(* -7 5 4 -3 0
7 -5 -4 3 0
7 -5 4 3 0
-7 5 4 3 0
3 -2 1 0
-3 2 1 0*)
let conjunction2_1 = start_process (exp_list2) (hord2) (tord2);;


