(* Question 1*)
let subset a b = List.for_all (fun x -> List.mem x b) a

(* Question 2*)
let equal_sets a b = subset a b && subset b a

(* Question 3*)
let set_union a b = a @ b

(* Question 4*)
let set_all_union a = List.fold_left set_union [] a

(* Question 5*)
(* 
   No set is a member of itself (by the Axiom of Foundation, which states that the universe of sets is well-founded). So the required function is the constant function [false].

   Alternatively, we could argue that the function cannot be defined because it is impossible to type a set containing itself (its type would be an infinite type 'a ... list list list list ... list, which is disallowed in ocaml).
*)
let self_member s = false

(* Question 6 *)
let rec computed_fixed_point eq f x =
  if eq (f x) x then x else computed_fixed_point eq f (f x)

(* Question 7 *)
let apply_n_times f n =
  let rec aux i acc =
    if i <= 0 then acc else aux (i - 1) (fun x -> f (acc x))
  in
  aux n (fun x -> x)

let computed_periodic_point eq f p x =
  let y = apply_n_times f p x in
  let rec find_periodic_point a b =
    if eq a b then a else find_periodic_point (f a) (f b)
  in
  find_periodic_point x y

(* Question 8 *)
let rec whileseq s p x = if not (p x) then [] else x :: whileseq s p (s x)

(* Question 9 *)
type ('nonterminal, 'terminal) symbol = N of 'nonterminal | T of 'terminal

(* returns [true] if symbol [symb] matches the left hand side of a given rule *)
let matches_rule symb = function lhs, _ -> symb = lhs

(* returns [true] if [symb] is a "safe" nonterminal, using currently valid rule
   @params: [symb]: a symbol
            a list of currently valid rules
*)
let rec is_safe_nonterminal symb = function
  | [] -> false
  | rule :: rs -> matches_rule symb rule || is_safe_nonterminal symb rs

(* returns [true] if input symbol can be proved to be "safe" using currently valid rules *)
let is_safe_symbol valid_rules = function
  | T _ -> true
  | N s -> is_safe_nonterminal s valid_rules

(* returns [true] if a rule can be proved to be valid using currently valid rules *)
let rec is_safe_rule valid_rules = function
  | _, rhs -> List.for_all (is_safe_symbol valid_rules) rhs

(* returns a sublist of [rules] containing those rules that can be proved to be valid using currently valid rules *)
let expand_valid_rules rules valid_rules =
  List.filter (is_safe_rule valid_rules) rules

let filter_blind_alleys (symb, rules) =
  (symb, computed_fixed_point equal_sets (expand_valid_rules rules) [])
