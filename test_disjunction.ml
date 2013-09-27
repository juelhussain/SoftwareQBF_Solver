let h1 = Hashtbl.create 15;;
let t1 = Hashtbl.create 15;;
let exp2 = (Or(Or(Var "95",Var "89"), Var "94"));;
let exp22 = (Or(Or(Var "91",Var "93"), Var "90"));;

let bdd2 = build (exp2) (h1) (t1);;
	(*bdd = Node (89, Node (94, Node (95, Zero, One), One), One)*)
let bdd22 = build (exp22) (h1) (t1);;
	(*Output: bdd = Node (90, Node (91, Node (93, Zero, One), One), One)*)
let bdd_dn2 = disjunction ([bdd2;bdd22]) ([exp2;exp22]) (h1) (t1);;
	(*Node (89,
     Node (90,
      Node (91, Node (93, Node (94, Node (95, Zero, One), One), One), One),
      One),
     One)*)
		
		
		(*Comparing exponential build with segmented disjunction*)
		let long_h = Hashtbl.create 15;;
		let long_t = Hashtbl.create 15;;
		
		let long_or = string_to_disjunction "1 32 2 13 3 33 4 5 12 6 7 8 11 9 10 14 41 19"
		(*Output:val long_or : expression =
  Or (Or (Or (Or (Or   (Or   (Or  (Or (Or (Or  (Or  (Or  (Or  (Or
                             (Or (Or (Or (Var "1", Var "32"), Var "2"),
                               Var "13"), Var "3"), Var "33"),Var "4"), Var "5"),
                     Var "12"),
                   Var "6"),
                 Var "7"),
               Var "8"),
             Var "11"),
           Var "9"),
         Var "10"),
       Var "14"),
     Var "41"),
   Var "19")*)
		
		
		let long_build = time build (long_or) (long_h) (long_t);;
		(*Output: Execution time: 26.289528s
      val long_build : bdd =
        Node (1,
         Node (2,
          Node (3,
           Node (4,
            Node (5,
             Node (6,
              Node (7,
               Node (8,
                Node (9,
                 Node (10,
                  Node (11,
                   Node (12,
                    Node (13,
                     Node (14,
                      Node (19,
                       Node (32, Node (33, Node (41, Zero, One), One), One), One),
                      One),
                     One),
                    One),
                   One),
                  One),
                 One),
                One),
               One),
              One),
             One),
            One),
           One),
          One),
         One)*)
	
	let short_or1 = string_to_disjunction "1 32 2 13 3 33 4 5 12"
	(*Output: short_or1 : expression =
  Or
   (Or
     (Or
       (Or
         (Or (Or (Or (Or (Var "1", Var "32"), Var "2"), Var "13"), Var "3"),
         Var "33"),
       Var "4"),
     Var "5"),
   Var "12")*)
	
	let short_or2 = string_to_disjunction "6 7 8 11 9 10 14 41 19"
	(*val short_or2 : expression =
  Or
   (Or
     (Or
       (Or (Or (Or (Or (Or (Var "6", Var "7"), Var "8"), Var "11"), Var "9"),
         Var "10"),
       Var "14"),
     Var "41"),
   Var "19")*)
	let short_h1 = Hashtbl.create 15;;
	let short_t1 = Hashtbl.create 15;;
	let short_build1 = time build (short_or1) (short_h1) (short_t1);;
	(*Execution time: 0.034991s
			val short_build1 : bdd =
        Node (1,Node (2,Node (3,Node (4,Node (5,
             Node (12, Node (13, Node (32, Node (33, Zero, One), One), One), One),
             One),One), One),One), One)*)
									
	let short_build2 = time build (short_or2) (short_h1) (short_t1);;
  (*	Execution time: 0.020737s
  val short_build2 : bdd =
    Node (6,Node (7,Node (8,Node (9, Node (10,
         Node (11, Node (14, Node (19, Node (41, Zero, One), One), One), One),
         One), One), One), One),One)*)
				
	let combine_dis = time disjunction ([short_build1;short_build2]) ([short_or1;short_or2]) (short_h1) (short_t1);;
	(*Output:	Execution time: 0.163729s
val combine_dis : bdd =
  Node (1,
   Node (2,
    Node (3,
     Node (4,
      Node (5,
       Node (6,
        Node (7,
         Node (8,
          Node (9,
           Node (10,
            Node (11,
             Node (12,
              Node (13,
               Node (14,
                Node (19,
                 Node (32, Node (33, Node (41, Zero, One), One), One), One),
                One),
               One),
              One),
             One),
            One),
           One),
          One),
         One),
        One),
       One),
      One),
     One),
    One),
   One)*)

let h1=Hashtbl.create 15;;
let t1 = Hashtbl.create 15;;

let string_clause = "1 2 3 4 5 6 7 8 9 10"

let dis_clause = get_clause_disjunct string_clause 4 h1 t1
(*val dis_clause : bdd =
  Node (1,
   Node (2,
    Node (3,
     Node (4,
      Node (5,
       Node (6,
        Node (7, Node (8, Node (9, Node (10, Zero, One), One), One), One),
        One),
       One),
      One),
     One),
    One),
   One)
# *)

let h2 = Hashtbl.create 15;;
let t2 = Hashtbl.create 15;;
let dis_clause2 = get_clause_disjunct string_clause 2 h2 t2
dis_clause=dis_clause2;;(*bool = true*)


let h3 = Hashtbl.create 15;;
let t3 = Hashtbl.create 15;;
let dis_clause3 = get_clause_disjunct string_clause 5 h3 t3

let h4 = Hashtbl.create 15;;
let t4 = Hashtbl.create 15;;
let dis_clause4 = get_clause_disjunct string_clause 8 h4 t4



	let new_combine_dis = time2 (start_process_segmentation_double) 
	(["1 32 2 13 3 33 4 5 12 6 7 8 11 9 10 14 41 19"]) (long_h) (long_t) (2) (4);;
	(*Execution time: 52.746959s*)


	let new_combine_dis2 = time4 "disjunction segmentation" get_clause_disjunct "1 32 2 13 3 33 4 5 12 6 7 8 11 9 10 14 41 19" 4 long_h1 long_t1;;
	(*Execution time: 0.222854s*)
				
	new_combine_dis2=List.nth new_combine_dis 0;;
	(*bool = true*)
				
	new_combine_dis2=long_build
	(*bool = true*)
				
	List.nth new_combine_dis 0=long_build
	(*bool = true*)
				
				



	