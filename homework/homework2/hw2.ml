(* Question 1 *)

let sort_rules rules = List.sort (fun x y -> Stdlib.compare (fst x) (fst y)) rules

(* returns a list of key-value pairs where keys are nonterminal symbols
   and values are alternative lists. Expects a list of rules with contiguous
   symbols. *)
let map_from_rules sorted_rules =
  let rec aux acc curr = function
    | [] -> []
    | [ (s, rhs) ] -> (s, List.rev (rhs :: curr)) :: acc
    | (s1, rhs1) :: ((s2, rhs2) :: _ as t) ->
      if s1 = s2
      then aux acc (rhs1 :: curr) t
      else aux ((s1, List.rev (rhs1 :: curr)) :: acc) [] t
  in
  List.rev (aux [] [] sorted_rules)
;;

(* Given a symbol and map of (symbol, alternatives) pairs, returns the alternatives list corresponding to the symbol *)
let rec retrieve_alternatives s = function
  | [] -> []
  | (lhs, alternatives) :: xs ->
    if s = lhs then alternatives else retrieve_alternatives s xs
;;

(* Implements the convention that prod_fun symb = [[]] for a symbol with no 
   corresponding rules*)
let correct_prod_func = function
  | [] -> [ [] ]
  | x -> x
;;

(* Given a map of (symbol, alternatives) pairs, returns a function mapping nonterminals to their alternatives lists *)
let grammar_function_from_map map = fun s -> retrieve_alternatives s map

(* Returns the production function corresponding to the given rules *)
let make_prod_func rules = grammar_function_from_map (map_from_rules (sort_rules rules))

(* given a hw1 grammar, returns a hw2 grammar *)
let convert_grammar (symbol, rules) = symbol, make_prod_func rules

(* Question 2 *)
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

(* Question 4 *)
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let parse_terminal t frag accept =
  match frag with
  | [] -> None
  | x :: tail -> if x = t then accept (Leaf x) tail else None
;;

let rec parse_symbol prod_func s frag accept =
  match s with
  | T t -> parse_terminal t frag accept
  | N nt -> parse_nonterminal prod_func nt accept frag

and parse_nonterminal prod_func nt accept frag =
  let rules = prod_func nt |> correct_prod_func in
  let rec try_rules = function
    | [] -> None
    | rule :: rest ->
      (match
         parse_rule prod_func rule frag (fun trees suffix ->
           accept (Node (nt, trees)) suffix)
       with
       | Some tree -> Some tree
       | _ -> try_rules rest)
  in
  try_rules rules

and parse_rule prod_func rule frag accept =
  match rule with
  | [] -> accept [] frag
  | symb :: rest ->
    parse_symbol prod_func symb frag (fun tree_node suffix ->
      parse_rule prod_func rest suffix (fun node_list suffix' ->
        accept (tree_node :: node_list) suffix'))
;;

let accept_empty_suffix tree = function
  | [] -> Some tree
  | _ -> None
;;

let make_parser (start_symbol, prod_func) =
  parse_nonterminal prod_func start_symbol accept_empty_suffix
;;

(* Question 3 *)
let wrap_acceptor matcher_accept = fun tree -> matcher_accept

let make_matcher (start_symbol, prod_func) =
  fun accept -> parse_nonterminal prod_func start_symbol (wrap_acceptor accept)
;;
