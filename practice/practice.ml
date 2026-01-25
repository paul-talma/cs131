let enforced_sum xy =
  match xy with
  | None, _ -> None
  | Some x, None -> Some x
  | Some x, Some y -> Some (x + y)
;;

let rec length list =
  match list with
  | [] -> 0
  | x :: xs -> 1 + length xs
;;

let tail_length list =
  let rec aux acc = function
    | [] -> acc
    | _ :: xs -> aux (acc + 1) xs
  in
  aux 0 list
;;

let rec my_rev = function
  | [] -> []
  | h :: t -> my_rev t @ [ h ]
;;

let my_tail_rev list =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (h :: acc) t
  in
  aux [] list
;;

let palindrome list = list = my_tail_rev list

let bogo_palindrome list =
  let rev_list = my_tail_rev list in
  let rec aux l1 l2 =
    match l1, l2 with
    | [], _ | _, [] -> true
    | h1 :: t1, h2 :: t2 -> h1 = h2 && aux t1 t2
  in
  aux list rev_list
;;

let my_filter p list =
  let rec aux acc = function
    | [] -> my_tail_rev acc
    | h :: t -> aux (if p h then h :: acc else acc) t
  in
  aux [] list
;;

let rec my_map f = function
  | [] -> []
  | h :: t -> f h :: my_map f t
;;

let my_map_tail f list =
  let rec aux acc = function
    | [] -> my_tail_rev acc
    | h :: t -> aux (f h :: acc) t
  in
  aux [] list
;;

let rec my_fold_left f init list =
  match list with
  | [] -> init
  | h :: t -> my_fold_left f (f init h) t
;;

let rec my_fold_right f list init =
  match list with
  | [] -> init
  | h :: t -> f h (my_fold_right f t init)
;;

(*
foldl f init [x1; x2; x3; ...; xn] = f (... (f (f init x1) x2) ... ) xn

foldr f [x1;x2;...;xn] init = f x1 (f x2 (f ... (f xn init)))
*)

let my_map_fold f list = my_fold_right (fun e acc -> f e :: acc) list []

let rec my_rev_append l1 l2 =
  match l1 with
  | [] -> l2
  | h :: t -> my_rev_append t (h :: l2)
;;

let my_concat_map f list =
  let rec aux acc = function
    | [] -> my_tail_rev acc
    | h :: t -> aux (my_rev_append (f h) acc) t
  in
  aux [] list
;;

let my_map_concat f list = my_concat_map (fun x -> [ f x ]) list
let my_concat list = my_concat_map (fun x -> x) list
let my_filter_concat_map p ls = my_concat_map (fun x -> if p x then [ x ] else []) ls

let rec my_exists f = function
  | [] -> false
  | h :: t -> f h || my_exists f t
;;

let my_exists_fold f list = my_fold_left (fun acc x -> acc || f x) false list

let rec my_find_opt f = function
  | [] -> None
  | h :: t -> if f h then Some (f h) else my_find_opt f t
;;

let my_find_opt_fold f =
  my_fold_left
    (fun acc x ->
       match acc with
       | None -> if f x then Some x else None
       | Some y -> acc)
    None
;;

let my_partition f list =
  my_fold_right (fun x (l1, l2) -> if f x then x :: l1, l2 else l1, x :: l2) list ([], [])
;;

let rec my_combine l1 l2 =
  match l1, l2 with
  | [], [] -> []
  | h1 :: t1, h2 :: t2 -> (h1, h2) :: my_combine t1 t2
  | _, _ -> raise (Invalid_argument "lists must have the same lenght")
;;

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
