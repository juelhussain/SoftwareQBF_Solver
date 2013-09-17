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