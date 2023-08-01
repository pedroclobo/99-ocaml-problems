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

(* Problem 05 - Reverse a List *)
let rev (lst : 'a list) : 'a list =
  let rec rev_aux (lst : 'a list) (res : 'a list) =
    match lst with [] -> res | e :: rest -> rev_aux rest (e :: res)
  in
  rev_aux lst []

let () =
  assert (rev [ "a"; "b"; "c" ] = [ "c"; "b"; "a" ]);
  assert (rev [] = [])

(* Problem 06 - Palindrome *)
let is_palindrome (lst : 'a list) : bool = lst = rev lst

let () =
  assert (is_palindrome [ "x"; "a"; "m"; "a"; "x" ]);
  assert (not (is_palindrome [ "a"; "b" ]))

(* Problem 08 - Eliminate Duplicates *)
let rec compress (lst : 'a list) : 'a list =
  let fst (lst : 'a list) : 'a option =
    match lst with [] -> None | x :: _ -> Some x
  in
  match lst with
  | [] -> []
  | x :: rest when Some x = fst rest -> compress rest
  | x :: rest -> x :: compress rest

let () =
  assert (
    compress
      [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
    = [ "a"; "b"; "c"; "a"; "d"; "e" ]);
  assert (compress [] = [])

(* Problem 14 - Duplicate the Elements of a List *)
let rec duplicate (lst : 'a list) : 'a list =
  match lst with [] -> [] | x :: rest -> x :: x :: duplicate rest

let () =
  assert (
    duplicate [ "a"; "b"; "c"; "c"; "d" ]
    = [ "a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d" ]);
  assert (duplicate [] = [])
