let time f x =
    let t = Sys.time() in
    let fx = f x in
    Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
    fx
		
let time f w x y z =
    let t = Sys.time() in
    let fwxyz = f w x y z in
    Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
    fwxyz	
		
let time f x y z =
  let t = Sys.time() in
  let fxyz = f x y z in
  Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
  fxyz	


let time f x =
    let start = Unix.gettimeofday ()
    in let res = f x
    in let stop = Unix.gettimeofday ()
    in let () = Printf.printf "Execution time: %fs\n%!" (stop -. start)
    in
       res