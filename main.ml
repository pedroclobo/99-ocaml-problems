(* Problem 01 - Tail of a List *)
let rec last (lst : 'a list) : 'a option =
  match lst with [] -> None | [ e ] -> Some e | _ :: rest -> last rest

let () =
  assert (last [ "a"; "b"; "c"; "d" ] = Some "d");
  assert (last [] = None)
