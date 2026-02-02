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
  | _, _ -> raise (Invalid_argument "lists must have the same length")
;;

(* eliminate consequtive duplicates *)
let rec compress = function
  | x :: (y :: _ as t) -> if x = y then compress t else x :: compress t
  | x -> x
;;

(* pack consequtive duplicates *)
let pack list =
  let rec aux packs curr_pack = function
    | [] -> []
    | [ x ] -> (x :: curr_pack) :: packs
    | a :: (b :: _ as t) ->
      if a = b then aux packs (a :: curr_pack) t else aux ((a :: curr_pack) :: packs) [] t
  in
  List.rev (aux [] [] list)
;;

(* run length encoding *)
let encode list =
  let rec aux acc count = function
    | [] -> []
    | [ x ] -> (count + 1, x) :: acc
    | a :: (b :: _ as t) ->
      if a = b then aux acc (count + 1) t else aux ((count + 1, a) :: acc) 0 t
  in
  List.rev (aux [] 0 list)
;;

(* modified RLE *)
type 'a rle =
  | One of 'a
  | Many of int * 'a

let mod_encode list =
  let elem (count, e) = if count = 1 then One e else Many (count, e) in
  let rec aux acc count = function
    | [] -> []
    | [ x ] -> elem (count + 1, x) :: acc
    | a :: (b :: _ as t) ->
      if a = b then aux acc (count + 1) t else aux (elem (count + 1, a) :: acc) 0 t
  in
  List.rev (aux [] 0 list)
;;

(* binary tree *)
type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

let rec is_mirror t1 t2 =
  match t1, t2 with
  | Empty, Empty -> true
  | Empty, _ | _, Empty -> false
  | Node (_, l1, r1), Node (_, l2, r2) -> is_mirror l1 r2 && is_mirror l2 r1
;;

let test_is_mirror =
  is_mirror
    (Node (1, Node (2, Empty, Empty), Node (5, Empty, Empty)))
    (Node (1, Node (2, Node (3, Empty, Empty), Empty), Node (4, Empty, Empty)))
  = false
;;

let is_symmetric = function
  | Empty -> true
  | Node (_, l, r) -> is_mirror l r
;;

(* BSTs *)
let rec insert tree x =
  match tree with
  | Empty -> Node (x, Empty, Empty)
  | Node (y, l, r) ->
    if x = y
    then tree
    else if x < y
    then Node (y, insert l x, r)
    else Node (y, l, insert r x)
;;

let construct ls = List.fold_left insert Empty ls
let symm_test0 = is_symmetric (construct [ 5; 3; 18; 1; 4; 12; 21 ])
let symm_test1 = not (is_symmetric (construct [ 3; 2; 5; 7; 4 ]))

let count_leaves tree =
  let rec aux count = function
    | Node (_, Empty, Empty) -> count + 1
    | Node (_, l, r) -> aux count l + aux count r
    | Empty -> 0
  in
  aux 0 tree
;;

let count_test =
  count_leaves (Node (1, Node (1, Empty, Empty), Node (2, Node (3, Empty, Empty), Empty)))
  = 2
;;

let collect_leaves tree =
  let rec aux t acc =
    match t with
    | Empty -> acc
    | Node (x, Empty, Empty) -> x :: acc
    | Node (_, l, r) -> aux l (aux r acc)
  in
  aux tree []
;;

let collect_test =
  collect_leaves
    (Node (1, Node (1, Empty, Empty), Node (2, Node (3, Empty, Empty), Empty)))
  = [ 1; 3 ]
;;

let internals tree =
  let rec aux t acc =
    match t with
    | Empty -> acc
    | Node (_, Empty, Empty) -> acc
    | Node (x, l, r) -> aux l (x :: aux r acc)
  in
  aux tree []
;;

let internals_test =
  internals (Node (1, Node (1, Empty, Empty), Node (2, Node (3, Empty, Empty), Empty)))
;;

let at_level t l =
  let rec aux tree level acc =
    match tree with
    | Empty -> acc
    | Node (x, left, right) ->
      if level = 1 then x :: acc else aux left (level - 1) (aux right (level - 1) acc)
  in
  aux t l []
;;

let at_level_test =
  at_level
    (Node
       ( 'a'
       , Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty))
       , Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty)) ))
    2
;;

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

let parse_tree_leaves tree =
  let rec aux acc = function
    | Leaf t -> t :: acc
    | Node (n, ls) -> List.fold_left aux acc ls
  in
  List.rev (aux [] tree)
;;

let parse_leaves_test =
  parse_tree_leaves (Node (1, [ Node (2, [ Leaf 3; Leaf 4 ]) ])) = [ 3; 4 ]
;;
