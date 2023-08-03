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

(* Problem 10 - Run-Length Encoding *)
let encode (lst : 'a list) : (int * 'a) list =
  let rec map f lst =
    match lst with [] -> [] | x :: rest -> f x :: map f rest
  in
  map
    (fun l ->
      (length l, match last l with None -> failwith "Empty list" | Some x -> x))
    (pack lst)

(* Problem 11 - Modified Run-Length Encoding *)
type 'a rle = One of 'a | Many of int * 'a

let modifiedEncode (lst : 'a list) : 'a rle list =
  let rec aux (lst : (int * 'a) list) (res : 'a rle list) =
    match lst with
    | [] -> res
    | (n, x) :: rest ->
        if n = 1 then aux rest (res @ [ One x ])
        else aux rest (res @ [ Many (n, x) ])
  in
  aux (encode lst) []

(* Problem 12 - Decode a Run-Length Encoded List *)
let decode (lst : 'a rle list) : 'a list =
  let rec aux (lst : 'a rle list) (res : 'a list) =
    let rec replicate (e : 'a) (n : int) =
      match n with
      | n when n <= 0 -> []
      | 1 -> [ e ]
      | _ -> e :: replicate e (n - 1)
    in
    match lst with
    | [] -> res
    | One x :: rest -> aux rest (res @ [ x ])
    | Many (n, x) :: rest -> aux rest (res @ replicate x n)
  in
  aux lst []

(* Problem 13 - Run-Length Encoding of a List (Direct Solution) *)
let directEncode (lst : 'a list) : 'a rle list =
  let rec aux (lst : 'a list) (res : 'a rle list) (el : 'a option) (acc : int) =
    let count (x : 'a) (count : int) : 'a rle =
      if count = 1 then One x else Many (count, x)
    in
    match (lst, el) with
    | [], None -> res
    | [], Some v -> res @ [ count v acc ]
    | x :: rest, None -> aux rest res (Some x) (acc + 1)
    | x :: rest, Some v when v = x -> aux rest res (Some x) (acc + 1)
    | x :: rest, Some v -> aux rest (res @ [ count v acc ]) (Some x) 1
  in

  aux lst [] None 0

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

(* Problem 17 - Split a List Into Two Parts; The Length of the First Part Is Given *)
let split (lst : 'a list) (len : int) : 'a list * 'a list =
  let rec aux (lst : 'a list) (len : int) (res : 'a list * 'a list) (i : int) =
    match lst with
    | [] -> res
    | x :: rest ->
        if i > len then
          let a, b = res in
          aux rest len (a, b @ [ x ]) (i + 1)
        else
          let a, b = res in
          aux rest len (a @ [ x ], b) (i + 1)
  in
  aux lst len ([], []) 1

(* Problem 18 - Extract a Slice From a List *)
let slice (lst : 'a list) (i : int) (k : int) : 'a list =
  let rec aux (lst : 'a list) (i : int) (k : int) (res : 'a list) (count : int)
      =
    match lst with
    | [] -> res
    | x :: rest ->
        if i <= count && count <= k then aux rest i k (res @ [ x ]) (count + 1)
        else aux rest i k res (count + 1)
  in
  aux lst i k [] 0

(* Problem 19 - Rotate a List N Places to the Left *)
let rotate (lst : 'a list) (n : int) : 'a list =
  let rec aux (lst : 'a list) (i : int) =
    if i >= n then lst
    else match lst with [] -> [] | x :: rest -> aux (rest @ [ x ]) (i + 1)
  in
  aux lst 0
