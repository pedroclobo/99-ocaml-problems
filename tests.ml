#use "solutions.ml"

(* Problem 01 - Tail of a List *)
let () =
  assert (last [ "a"; "b"; "c"; "d" ] = Some "d");
  assert (last [] = None)

(* Problem 02 - Last Two Elements of a List *)
let () =
  assert (last_two [ "a"; "b"; "c"; "d" ] = Some ("c", "d"));
  assert (last_two [ "a" ] = None);
  assert (last_two [] = None)

(* Problem 03 - N'th Element of a List *)
let () =
  assert (nth [ "a"; "b"; "c"; "d" ] 1 = Some "a");
  assert (nth [ 1; 2; 3 ] 3 = Some 3);
  assert (nth [ "a"; "b"; "c"; "d" ] 0 = None);
  assert (nth [] 1 = None)

(* Problem 04 - Length of a List *)
let () =
  assert (length [ "a"; "b"; "c" ] = 3);
  assert (length [] = 0)

(* Problem 05 - Reverse a List *)
let () =
  assert (rev [ "a"; "b"; "c" ] = [ "c"; "b"; "a" ]);
  assert (rev [] = [])

(* Problem 06 - Palindrome *)
let () =
  assert (is_palindrome [ "x"; "a"; "m"; "a"; "x" ]);
  assert (not (is_palindrome [ "a"; "b" ]))

(* Problem 07 - Flatten a List *)
let () =
  assert (
    flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
    = [ "a"; "b"; "c"; "d"; "e" ])

(* Problem 08 - Eliminate Duplicates *)
let () =
  assert (
    compress
      [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
    = [ "a"; "b"; "c"; "a"; "d"; "e" ]);
  assert (compress [] = [])

(* Problem 09 - Pack Consecutive Duplicates *)
let () =
  assert (
    pack
      [
        "a";
        "a";
        "a";
        "a";
        "b";
        "c";
        "c";
        "a";
        "a";
        "d";
        "d";
        "e";
        "e";
        "e";
        "e";
      ]
    = [
        [ "a"; "a"; "a"; "a" ];
        [ "b" ];
        [ "c"; "c" ];
        [ "a"; "a" ];
        [ "d"; "d" ];
        [ "e"; "e"; "e"; "e" ];
      ]);
  assert (pack [] = [])

(* Problem 10 - Run-Length Encoding *)
let () =
  assert (
    encode
      [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
    = [ (4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e") ]);
  assert (encode [] = [])

(* Problem 11 - Modified Run-Length Encoding *)
let () =
  assert (
    modifiedEncode
      [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
    = [
        Many (4, "a");
        One "b";
        Many (2, "c");
        Many (2, "a");
        One "d";
        Many (4, "e");
      ]);
  assert (modifiedEncode [] = [])

(* Problem 12 - Decode a Run-Length Encoded List *)
let () =
  assert (
    decode
      [
        Many (4, "a");
        One "b";
        Many (2, "c");
        Many (2, "a");
        One "d";
        Many (4, "e");
      ]
    = [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]);
  assert (decode [] = [])

(* Problem 13 - Run-Length Encoding of a List (Direct Solution) *)
let () =
  assert (
    directEncode
      [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
    = [
        Many (4, "a");
        One "b";
        Many (2, "c");
        Many (2, "a");
        One "d";
        Many (4, "e");
      ]);
  assert (directEncode [] = [])

(* Problem 14 - Duplicate the Elements of a List *)
let () =
  assert (
    duplicate [ "a"; "b"; "c"; "c"; "d" ]
    = [ "a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d" ]);
  assert (duplicate [] = [])

(* Problem 15 - Replicate the Elements of a List a Given Number of Times *)
let () =
  assert (
    replicate [ "a"; "b"; "c" ] 3
    = [ "a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c" ]);
  assert (replicate [ "a"; "b"; "c" ] 0 = []);
  assert (replicate [] 1 = []);
  assert (replicate [] 3 = [])

(* Problem 16 - Drop Every N'th Element From a List *)
let () =
  assert (
    drop [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
    = [ "a"; "b"; "d"; "e"; "g"; "h"; "j" ]);
  assert (drop [ 1; 2; 3 ] 0 = []);
  assert (drop [ 1; 2; 3 ] 4 = [ 1; 2; 3 ]);
  assert (drop [] 1 = [])

(* Problem 17 - Split a List Into Two Parts; The Length of the First Part Is Given *)
let () =
  assert (
    split [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
    = ([ "a"; "b"; "c" ], [ "d"; "e"; "f"; "g"; "h"; "i"; "j" ]));
  assert (split [ "a"; "b"; "c"; "d" ] 5 = ([ "a"; "b"; "c"; "d" ], []));
  assert (split [ "a"; "b"; "c"; "d" ] 0 = ([], [ "a"; "b"; "c"; "d" ]));
  assert (split [] 1 = ([], []))

(* Problem 18 - Extract a Slice From a List *)
let () =
  assert (
    slice [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 2 6
    = [ "c"; "d"; "e"; "f"; "g" ])

(* Problem 19 - Rotate a List N Places to the Left *)
let () =
  assert (
    rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3
    = [ "d"; "e"; "f"; "g"; "h"; "a"; "b"; "c" ])

(* Problem 20 - Remove the K'th Element From a List *)
let () =
  assert (remove_at 1 [ "a"; "b"; "c"; "d" ] = [ "a"; "c"; "d" ]);
  assert (remove_at 5 [ "a"; "b"; "c"; "d" ] = [ "a"; "b"; "c"; "d" ]);
  assert (remove_at (-1) [ "a"; "b"; "c"; "d" ] = [ "a"; "b"; "c"; "d" ]);
  assert (remove_at 0 [] = [])

(* Problem 21 - Insert an Element at a Given Position Into a List *)
let () =
  assert (insert_at "z" 1 [ "a"; "b"; "c"; "d" ] = [ "a"; "z"; "b"; "c"; "d" ]);
  assert (insert_at "z" 4 [ "a"; "b"; "c"; "d" ] = [ "a"; "b"; "c"; "d"; "z" ]);
  assert (insert_at "z" 9 [ "a"; "b"; "c"; "d" ] = [ "a"; "b"; "c"; "d"; "z" ]);
  assert (insert_at "z" 0 [] = [ "z" ])
