


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