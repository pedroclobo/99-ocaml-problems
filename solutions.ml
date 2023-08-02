(* Problem 01 - Tail of a List *)
let rec last (lst : 'a list) : 'a option =
  match lst with [] -> None | [ e ] -> Some e | _ :: rest -> last rest

(* Problem 02 - Last Two Elements of a List *)
let rec last_two (lst : 'a list) : ('a * 'a) option =
  match lst with
  | [ x; y ] -> Some (x, y)
  | _ :: rest -> last_two rest
  | _ -> None

(* Problem 03 - N'th Element of a List *)
let rec nth (lst : 'a list) (n : int) : 'a option =
  match lst with
  | [] -> None
  | e :: rest -> (
      match n with 1 -> Some e | n when n < 1 -> None | _ -> nth rest (n - 1))

(* Problem 04 - Length of a List *)
let length (lst : 'a list) : int =
  let rec length_aux (lst : 'a list) (n : int) =
    match lst with [] -> n | _ :: rest -> length_aux rest (n + 1)
  in
  length_aux lst 0

(* Problem 05 - Reverse a List *)
let rev (lst : 'a list) : 'a list =
  let rec rev_aux (lst : 'a list) (res : 'a list) =
    match lst with [] -> res | e :: rest -> rev_aux rest (e :: res)
  in
  rev_aux lst []

(* Problem 06 - Palindrome *)
let is_palindrome (lst : 'a list) : bool = lst = rev lst

(* Problem 07 - Flatten a List *)
type 'a node = One of 'a | Many of 'a node list

let rec flatten (lst : 'a node list) : 'a list =
  match lst with
  | [] -> []
  | One x :: rest -> x :: flatten rest
  | Many x :: rest -> flatten x @ flatten rest

(* Problem 08 - Eliminate Duplicates *)
let rec compress (lst : 'a list) : 'a list =
  let fst (lst : 'a list) : 'a option =
    match lst with [] -> None | x :: _ -> Some x
  in
  match lst with
  | [] -> []
  | x :: rest when Some x = fst rest -> compress rest
  | x :: rest -> x :: compress rest

(* Problem 09 - Pack Consecutive Duplicates *)
let pack (lst : 'a list) : 'a list list =
  let rec aux (lst : 'a list) (res : 'a list list) (curr : 'a list) =
    match (lst, curr) with
    | [], [] -> res
    | [], _ -> res @ [ curr ]
    | x :: rest, [] -> aux rest res (x :: curr)
    | x :: rest, c :: _ when x = c -> aux rest res (x :: curr)
    | x :: rest, _ -> aux rest (res @ [ curr ]) [ x ]
  in
  aux lst [] []

(* Problem 14 - Duplicate the Elements of a List *)
let rec duplicate (lst : 'a list) : 'a list =
  match lst with [] -> [] | x :: rest -> x :: x :: duplicate rest

(* Problem 15 - Replicate the Elements of a List a Given Number of Times *)
let rec replicate (lst : 'a list) (n : int) : 'a list =
  let rec replicate_element (e : 'a) (n : int) =
    match n with
    | n when n <= 0 -> []
    | 1 -> [ e ]
    | _ -> e :: replicate_element e (n - 1)
  in
  match lst with
  | [] -> []
  | x :: rest -> (
      match n with
      | n when n <= 0 -> []
      | _ -> replicate_element x n @ replicate rest n)

(* Problem 16 - Drop Every N'th Element From a List *)
let drop (lst : 'a list) (n : int) : 'a list =
  let rec aux (lst : 'a list) (n : int) (i : int) (res : 'a list) =
    if n <= 0 then []
    else
      match (lst, i) with
      | [], _ -> res
      | _ :: rest, 1 -> aux rest n n res
      | x :: rest, _ -> aux rest n (i - 1) (res @ [ x ])
  in
  aux lst n n []
