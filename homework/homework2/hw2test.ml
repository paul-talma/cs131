(* 
   This file includes a lot of definitions as there is a type error while importing
   hw2.ml. No error is raised if all the definitions are in the same file.
   The tests begin on line 216.
*)

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let correct_prod_func = function
  | [] -> [ [] ]
  | x -> x
;;

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

let wrap_acceptor matcher_accept = fun tree -> matcher_accept

let make_matcher (start_symbol, prod_func) =
  fun accept -> parse_nonterminal prod_func start_symbol (wrap_acceptor accept)
;;

type json_nonterminal =
  | Value
  | Object
  | Array
  | Members
  | Member
  | Elements
  | Element
  | String
  | Characters
  | Character
  | Number
  | WS

let json_subset_grammar =
  ( Element
  , function
    | Value -> [ [ N Object ]; [ N Array ]; [ N String ]; [ N Number ] ]
    | Object -> [ [ T "{"; N WS; T "}" ]; [ T "{"; N Members; T "}" ] ]
    | Members -> [ [ N Member ]; [ N Member; T ","; N Members ] ]
    | Member -> [ [ N String; T ":"; N Element ] ]
    | Array -> [ [ T "["; N WS; T "]" ]; [ T "["; N Elements; T "]" ] ]
    | Elements -> [ [ N Element ]; [ N Element; T ","; N Elements ] ]
    | Element -> [ [ N Value ] ]
    | String -> [ [ T "\""; N Characters; T "\"" ] ]
    | Characters -> [ []; [ N Character; N Characters ] ]
    | Character ->
      [ [ T "a" ]
      ; [ T "b" ]
      ; [ T "c" ]
      ; [ T "d" ]
      ; [ T "e" ]
      ; [ T "f" ]
      ; [ T "g" ]
      ; [ T "h" ]
      ; [ T "i" ]
      ; [ T "j" ]
      ; [ T "k" ]
      ; [ T "l" ]
      ; [ T "m" ]
      ; [ T "n" ]
      ; [ T "o" ]
      ; [ T "p" ]
      ; [ T "q" ]
      ; [ T "r" ]
      ; [ T "s" ]
      ; [ T "t" ]
      ; [ T "u" ]
      ; [ T "v" ]
      ; [ T "w" ]
      ; [ T "x" ]
      ; [ T "y" ]
      ; [ T "z" ]
      ; [ T "_" ]
      ]
    | Number ->
      [ [ T "0" ]
      ; [ T "1" ]
      ; [ T "2" ]
      ; [ T "3" ]
      ; [ T "4" ]
      ; [ T "5" ]
      ; [ T "6" ]
      ; [ T "7" ]
      ; [ T "8" ]
      ; [ T "9" ]
      ]
    | WS -> [ [ T "" ]; [ T " " ] ] )
;;

(*
    test json object:
    {
        "my_array": [1,2,3],
        "my_object": {
            "my_string": "hi"
        }
    }
*)
let json_test_object =
  [ "{"
  ; "\""
  ; "m"
  ; "y"
  ; "_"
  ; "a"
  ; "r"
  ; "r"
  ; "a"
  ; "y"
  ; "\""
  ; ":"
  ; "["
  ; "1"
  ; ","
  ; "2"
  ; ","
  ; "3"
  ; "]"
  ; ","
  ; "\""
  ; "m"
  ; "y"
  ; "_"
  ; "o"
  ; "b"
  ; "j"
  ; "e"
  ; "c"
  ; "t"
  ; "\""
  ; ":"
  ; "{"
  ; "\""
  ; "m"
  ; "y"
  ; "_"
  ; "s"
  ; "t"
  ; "r"
  ; "i"
  ; "n"
  ; "g"
  ; "\""
  ; ":"
  ; "\""
  ; "h"
  ; "i"
  ; "\""
  ; "}"
  ; "}"
  ]
;;

let matcher_accept_empty = function
  | [] -> Some []
  | _ -> None
;;

let test_json_matcher0 =
  make_matcher json_subset_grammar matcher_accept_empty json_test_object = Some []
;;

let json_test_all = [ "1"; "2" ]
let matcher_accept_all x = Some x

let test_json_matcher1 =
  make_matcher json_subset_grammar matcher_accept_all json_test_all = Some [ "2" ]
;;

let test_json_matcher2 =
  make_matcher json_subset_grammar matcher_accept_empty json_test_all = None
;;

let parse_tree_leaves tree =
  let rec aux acc = function
    | Leaf t -> t :: acc
    | Node (n, ls) -> List.fold_left aux acc ls
  in
  List.rev (aux [] tree)
;;

let test_json_parser =
  match make_parser json_subset_grammar json_test_object with
  | None -> false
  | Some tree -> parse_tree_leaves tree = json_test_object
;;

(* tests for left recursive grammars *)
let left_rec_grammar =
  ( Value
  , function
    | Value -> [ [ N Value; T "0" ]; [ T "0"; T "0" ] ]
    | _ -> [] )
;;

let left_rec_expression = [ "0"; "0" ]

let test_left_rec_parser =
  match make_parser left_rec_grammar left_rec_expression with
  | None -> false
  | Some tree -> parse_tree_leaves tree = left_rec_expression
;;
