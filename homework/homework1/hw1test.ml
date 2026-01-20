let my_subset_test0 = subset [ 1; 1; 1; 2; 2; 3 ] [ 3; 2; 1 ]
let my_equal_sets_test0 = equal_sets [ 1; 1; 1; 2 ] [ 2; 2; 2; 1 ]
let my_equal_sets_test1 = not (equal_sets [ 1 ] [])

let my_set_union_test0 =
  equal_sets (set_union [ 1; 1; 1; 1 ] [ 4; 2; 3 ]) [ 1; 2; 2; 2; 2; 3; 4 ]

let my_set_all_union_test0 =
  equal_sets (set_all_union [ [ 1; 1; 1 ]; [ 2; 3; 4 ]; [ 7; 7; 7; 7 ] ]) [ 1; 2; 4; 3; 7 ]

let my_self_member_test0 = not (self_member [])
let my_self_member_test1 = not (self_member [ 1 ])
let my_self_member_test2 = not (self_member [ [ 1; 2; 3 ]; [ 4; 5 ]; [ 6 ] ])

let my_computed_fixed_point_test0 =
  computed_fixed_point ( = ) (fun x -> x *. x) 10. = infinity

let my_computed_fixed_point_test1 =
  computed_fixed_point equal_sets (fun s -> 0 :: s) [ 1; 2; 3 ] = [ 0; 1; 2; 3 ]

let my_computed_periodic_point_test0 =
  computed_periodic_point ( = ) (fun x -> -x) 2 10 = 10

let my_computed_periodic_point_test1 =
  computed_periodic_point ( = ) (fun x -> -x) 0 10 = 10

let my_whileseq_test0 = whileseq (fun x -> x + 1) (fun x -> x < 0) 0 = []

let my_whileseq_test1 =
  whileseq (fun x -> x + 1) (fun x -> x < 10) 0
  = [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 ]

type my_grammar_nonterminals = Expr | Val

let my_grammar_rules0 =
  [
    Expr, [ N Expr; T "+"; N Expr ];
    Val, [ T "0" ];
    Val, [ T "1" ];
    Val, [ T "2" ];
    Val, [ T "3" ];
  ]

let my_grammar0 = (Val, my_grammar_rules0)

let my_filter_blind_alleys_test0 =
  filter_blind_alleys my_grammar0
  = ( Val,
      [ Val, [T"0"];
        Val, [T"1"];
        Val, [T"2"];
        Val, [T"3"]]
    )

let my_grammar1 = (Expr, my_grammar_rules0)
let my_filter_blind_alleys_test1 =
  filter_blind_alleys my_grammar1
  = (Expr, [ ])

let my_grammar_rules2 =
    [Expr, [ N Val; N Val ];
     Val, [ N Expr; N Expr ]]
let my_grammar2 = (Val, my_grammar_rules2)

let my_filter_blind_alleys_test2 =
  filter_blind_alleys my_grammar2 = (Val, [])

let my_grammar_rules3 = []
let my_grammar3 = (Val, my_grammar_rules3)
let my_filter_blind_alleys_test3 = filter_blind_alleys my_grammar3 = my_grammar3
