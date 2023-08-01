(* Problem 01 - Tail of a List *)
let rec last (lst : 'a list) : 'a option =
  match lst with [] -> None | [ e ] -> Some e | _ :: rest -> last rest

let () =
  assert (last [ "a"; "b"; "c"; "d" ] = Some "d");
  assert (last [] = None)

(* Problem 02 - Last Two Elements of a List *)
let rec last_two (lst : 'a list) : ('a * 'a) option =
  match lst with
  | [ x; y ] -> Some (x, y)
  | _ :: rest -> last_two rest
  | _ -> None

let () =
  assert (last_two [ "a"; "b"; "c"; "d" ] = Some ("c", "d"));
  assert (last_two [ "a" ] = None);
  assert (last_two [] = None)

(* Problem 03 - N'th Element of a List *)
let rec nth (lst : 'a list) (n : int) : 'a option =
  match lst with
  | [] -> None
  | e :: rest -> (
      match n with 1 -> Some e | n when n < 1 -> None | _ -> nth rest (n - 1))

let () =
  assert (nth [ "a"; "b"; "c"; "d" ] 1 = Some "a");
  assert (nth [ 1; 2; 3 ] 3 = Some 3);
  assert (nth [ "a"; "b"; "c"; "d" ] 0 = None);
  assert (nth [] 1 = None)

(* Problem 04 - Length of a List *)
let length (lst : 'a list) : int =
  let rec length_aux (lst : 'a list) (n : int) =
    match lst with [] -> n | _ :: rest -> length_aux rest (n + 1)
  in
  length_aux lst 0

let () =
  assert (length [ "a"; "b"; "c" ] = 3);
  assert (length [] = 0)
