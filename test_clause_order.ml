let ht1 = Hashtbl.create 15;;

Hashtbl.add ht1 0 ["10";"10";"10"];;

Hashtbl.add ht1 1 ["2";"5";"0"];;

Hashtbl.add ht1 2 ["1";"7";"3"];;

Hashtbl.add ht1 3 ["3";"5";"0"];;

sum_vars_ht ht1;;



sum_vars_ht ht1;;

let ht_clause = Hashtbl.create 15;;
let ht_ord = Hashtbl.create 15;;

let clauseList_ord = ["1 2 3 4";"7 8 5 4";"1 2 4 -1";"-7 -8 4 5 1";"-2 3 4 -1"];;

order_clauses (clauseList_ord) (ht_ord) (ht_clause);;

let ht_clause2 = Hashtbl.create 15;;
let ht_ord2 = Hashtbl.create 15;;

let clauseList_ord2 = ["1 2 3 4";"4 4 4 4";"4 3 4 4";"-3 -5 2 5 -5";"-1 4 4 -1"];;
order_clauses (clauseList_ord2) (ht_ord2) (ht_clause2);;

let ht_clause3 = Hashtbl.create 15;;
let ht_ord3 = Hashtbl.create 15;;

let clauseList_ord3 = ["1 -2 3";"5 -5 4 3 -3";"2 -3 1 1";"4 4 -3 5"];;
order_clauses (clauseList_ord3) (ht_ord3) (ht_clause3);;




let ht_clause4 = Hashtbl.create 15;;
let ht_ord4 = Hashtbl.create 15;;

let clauseList_ord4 = ["1 -1 1 1";"2 -2 2 2 -2";"3 -3 2 3";"2 -2 2 2";"-1 1 1 -1 1"];;
(*The out put should be in order:*)
(* ["1 -1 1 1";"-1 1 1 -1 1";"2 -2 2 2 -2";"2 -2 2 2";"3 -3 1 3"] *)


order_clauses (clauseList_ord4) (ht_ord4) (ht_clause4);;







