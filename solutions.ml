(* Problem 01 - Tail of a List *)
let rec last (lst : 'a list) : 'a option =
  match lst with [] -> None | [ x ] -> Some x | _ :: tail -> last tail

(* Problem 02 - Last Two Elements of a List *)
let rec last_two (lst : 'a list) : ('a * 'a) option =
  match lst with
  | [ x; y ] -> Some (x, y)
  | _ :: tail -> last_two tail
  | _ -> None

(* Problem 03 - N'th Element of a List *)
let rec nth (lst : 'a list) (n : int) : 'a option =
  match lst with
  | [] -> None
  | head :: tail -> (
      match n with
      | 1 -> Some head
      | n when n < 1 -> None
      | _ -> nth tail (n - 1))

(* Problem 04 - Length of a List *)
let length (lst : 'a list) : int =
  let rec aux (lst : 'a list) (n : int) =
    match lst with [] -> n | _ :: tail -> aux tail (n + 1)
  in
  aux lst 0

(* Problem 05 - Reverse a List *)
let rev (lst : 'a list) : 'a list =
  let rec aux (lst : 'a list) (res : 'a list) =
    match lst with [] -> res | head :: tail -> aux tail (head :: res)
  in
  aux lst []

(* Problem 06 - Palindrome *)
let is_palindrome (lst : 'a list) : bool = lst = rev lst

(* Problem 07 - Flatten a List *)
type 'a node = One of 'a | Many of 'a node list

let rec flatten (lst : 'a node list) : 'a list =
  match lst with
  | [] -> []
  | One x :: tail -> x :: flatten tail
  | Many x :: tail -> flatten x @ flatten tail

(* Problem 08 - Eliminate Duplicates *)
let rec compress (lst : 'a list) : 'a list =
  let fst (lst : 'a list) : 'a option =
    match lst with [] -> None | head :: _ -> Some head
  in
  match lst with
  | [] -> []
  | head :: tail when Some head = fst tail -> compress tail
  | head :: tail -> head :: compress tail

(* Problem 09 - Pack Consecutive Duplicates *)
let pack (lst : 'a list) : 'a list list =
  let rec aux (lst : 'a list) (res : 'a list list) (curr : 'a list) =
    match (lst, curr) with
    | [], [] -> res
    | [], _ -> res @ [ curr ]
    | head :: tail, [] -> aux tail res (head :: curr)
    | head :: tail, c :: _ when head = c -> aux tail res (head :: curr)
    | head :: tail, _ -> aux tail (res @ [ curr ]) [ head ]
  in
  aux lst [] []

(* Problem 10 - Run-Length Encoding *)
let encode (lst : 'a list) : (int * 'a) list =
  let rec map f lst =
    match lst with [] -> [] | head :: tail -> f head :: map f tail
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
    | (n, x) :: tail ->
        let res = if n = 1 then res @ [ One x ] else res @ [ Many (n, x) ] in
        aux tail res
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
    | One x :: tail -> aux tail (res @ [ x ])
    | Many (n, x) :: tail -> aux tail (res @ replicate x n)
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
    | head :: tail, None -> aux tail res (Some head) (acc + 1)
    | head :: tail, Some v when v = head -> aux tail res (Some head) (acc + 1)
    | head :: tail, Some v -> aux tail (res @ [ count v acc ]) (Some head) 1
  in

  aux lst [] None 0

(* Problem 14 - Duplicate the Elements of a List *)
let rec duplicate (lst : 'a list) : 'a list =
  match lst with [] -> [] | head :: tail -> head :: head :: duplicate tail

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
  | head :: tail -> (
      match n with
      | n when n <= 0 -> []
      | _ -> replicate_element head n @ replicate tail n)

(* Problem 16 - Drop Every N'th Element From a List *)
let drop (lst : 'a list) (n : int) : 'a list =
  let rec aux (lst : 'a list) (i : int) (res : 'a list) =
    if n <= 0 then []
    else
      match (lst, i) with
      | [], _ -> res
      | _ :: tail, 1 -> aux tail n res
      | head :: tail, _ -> aux tail (i - 1) (res @ [ head ])
  in
  aux lst n []

(* Problem 17 - Split a List Into Two Parts; The Length of the First Part Is Given *)
let split (lst : 'a list) (len : int) : 'a list * 'a list =
  let rec aux (lst : 'a list) (res : 'a list * 'a list) (i : int) =
    match lst with
    | [] -> res
    | head :: tail ->
        if i > len then
          let a, b = res in
          aux tail (a, b @ [ head ]) (i + 1)
        else
          let a, b = res in
          aux tail (a @ [ head ], b) (i + 1)
  in
  aux lst ([], []) 1

(* Problem 18 - Extract a Slice From a List *)
let slice (lst : 'a list) (i : int) (k : int) : 'a list =
  let rec aux (lst : 'a list) (res : 'a list) (count : int) =
    match lst with
    | [] -> res
    | head :: tail ->
        if i <= count && count <= k then aux tail (res @ [ head ]) (count + 1)
        else aux tail res (count + 1)
  in
  aux lst [] 0

(* Problem 19 - Rotate a List N Places to the Left *)
let rotate (lst : 'a list) (n : int) : 'a list =
  let rec aux (lst : 'a list) (i : int) =
    if i >= n then lst
    else
      match lst with [] -> [] | head :: tail -> aux (tail @ [ head ]) (i + 1)
  in
  aux lst 0

(* Problem 20 - Remove the K'th Element From a List *)
let remove_at (k : int) (lst : 'a list) : 'a list =
  let rec aux (i : int) (lst : 'a list) (res : 'a list) : 'a list =
    match lst with
    | [] -> res
    | head :: tail ->
        if i = k then aux (i + 1) tail res else aux (i + 1) tail (res @ [ head ])
  in
  aux 0 lst []

(* Problem 21 - Insert an Element at a Given Position Into a List *)
let insert_at (e : 'a) (n : int) (lst : 'a list) : 'a list =
  let rec aux (i : int) (lst : 'a list) (res : 'a list) =
    match lst with
    | [] -> if i <= n then res @ [ e ] else res
    | head :: tail ->
        let res = if i = n then res @ [ e ] @ [ head ] else res @ [ head ] in
        aux (i + 1) tail res
  in
  aux 0 lst []
