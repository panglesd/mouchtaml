let compute () =
  let state = Model.create_state () in
  if Model.solve state then 1 else 0

let compute n =
  let rec aux i acc =
    Printf.printf "computing number %d\n%!" i;
    if i = n then acc else aux (i + 1) (acc + compute ())
  in
  aux 0 0

let n = 1000

let () =
  Printf.printf "Nombre de moustaches gagnante au premier tour : %d / %d"
    (compute n) n
