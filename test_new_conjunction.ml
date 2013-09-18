begin
    						match element with
    						| Zero -> Zero
    						| One -> conjunction' (drop nodesList m) (drop expList m) (m1 -1)
    						| _ -> 
    							begin
      							let maxVar = select_max_var (nodesList) in
    								let left =
    									get_left_bdd (nodesList) (expList) (maxVar) (h) (t) in
    								let right =
    									get_right_bdd (nodesList) (expList) (maxVar) (h) (t) in
    								let left_exp = get_left_exp (expList) (maxVar) in
    								let right_exp = get_right_exp (expList) (maxVar) in
    								let k1 = conjunction' (left) (left_exp) (List.length left) in
    								let k2 = conjunction' (right) (right_exp) (List.length right) in
    								make (maxVar) (k1) (k2) (h) (t)
        					end
    					end
							
							
		let rec conjunctionTest nodesList expList h t m=
			if (m<1) then begin print_string "less than one\n"; (List.nth nodesList 0) end
			else
			begin
				let element = (at m nodesList) in
				match element with
				| Zero -> print_string "zero\n";Zero
				| One -> conjunctionTest (drop nodesList m) (drop expList m) (h) (t) (m -1)
				| _ -> 
					begin
						let maxVar = select_max_var (nodesList) in
						let left =
							get_left_bdd (nodesList) (expList) (maxVar) (h) (t) in
						let right =
							get_right_bdd (nodesList) (expList) (maxVar) (h) (t) in
						let left_exp = get_left_exp (expList) (maxVar) in
						let right_exp = get_right_exp (expList) (maxVar) in
						let k1 = conjunctionTest (left) (left_exp) (h) (t) (List.length left) in
						let k2 = conjunctionTest (right) (right_exp) (h) (t) (List.length right) in
						make (maxVar) (k1) (k2) (h) (t)
					end
			end