(* tests *)
let my_map_tail_test0 = my_map_tail (fun x -> x * x) [ 1; 2; 3 ] = [ 1; 4; 9 ]
let my_fold_left_test0 = my_fold_left (fun acc x -> acc - x) 10 [ 1; 2; 3 ] = 4
let my_fold_right_test0 = my_fold_right (fun x acc -> x - acc) [ 1; 2; 3 ] 10 = -8
let my_map_fold_test0 = my_map_fold (fun x -> x * x) [ 2; 3; 4 ] = [ 4; 9; 16 ]
let my_rev_append_test0 = my_rev_append [ 1; 2; 3 ] [ 4; 5; 6 ] = [ 3; 2; 1; 4; 5; 6 ]

let my_concat_map_tail_test0 =
  my_concat_map (fun x -> [ x; x; x ]) [ 1; 2; 3 ] = [ 1; 1; 1; 2; 2; 2; 3; 3; 3 ]
;;

let my_map_concat_test0 = my_map_concat (fun x -> x * x) [ 2; 3; 4 ] = [ 4; 9; 16 ]
let my_concat_test = my_concat [ [ 1; 2; 3 ]; [ 7; 8; 9 ] ] = [ 1; 2; 3; 7; 8; 9 ]

let my_filter_concat_map_test0 =
  my_filter_concat_map (fun x -> x < 10) [ 1; 2; 3; 66; 55; 4 ] = [ 1; 2; 3; 4 ]
;;

let my_exists_test0 = my_exists (fun x -> x = 10) [ 1; 2; 3 ] = false
let my_exists_fold_test0 = my_exists_fold (fun x -> x = 10) [ 1; 2; 3 ] = false
let my_find_opt_test0 = my_find_opt (fun x -> x = 10) [ 1; 2; 3 ] = None
let my_find_opt_fold_test0 = my_find_opt_fold (fun x -> x = 10) [ 1; 2; 3 ] = None

let my_partition_test0 =
  my_partition (fun x -> x mod 2 = 0) [ 1; 2; 3; 4; 5; 6 ] = ([ 2; 4; 6 ], [ 1; 3; 5 ])
;;

let my_combine_text0 = my_combine [ 1; 2; 3 ] [ 4; 5; 6 ] = [ 1, 4; 2, 5; 3, 6 ]

let compress_test0 =
  compress [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  = [ "a"; "b"; "c"; "a"; "d"; "e" ]
;;

let pack_test0 =
  pack [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e" ]
  = [ [ "a"; "a"; "a"; "a" ]
    ; [ "b" ]
    ; [ "c"; "c" ]
    ; [ "a"; "a" ]
    ; [ "d"; "d" ]
    ; [ "e"; "e"; "e"; "e" ]
    ]
;;

let encode_test0 = encode [ "a"; "a" ] = [ 2, "a" ]

let encode_test1 =
  encode [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  = [ 4, "a"; 1, "b"; 2, "c"; 2, "a"; 1, "d"; 4, "e" ]
;;

let mod_encode_test0 =
  mod_encode [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  = [ Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e") ]
;;
