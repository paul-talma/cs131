sum(0, Y, Y).
sum(X, Y, Z) :- X1 is X - 1, Z1 is Z -1, sum(X1, Y, Z1), !.

factorial(0, 1) :-
	!.
factorial(X, Y) :-
	X > 0,
	X1 is X - 1,
	factorial(X1, Y1),
	Y is X * Y1.

my_max(X, Y, X) :- X >= Y.
my_max(X, Y, Y) :- X < Y.

maxlist([X], X).
maxlist([H | T], M) :-
	maxlist(T, MT),
	my_max(H, MT, M), 
	!.

ordered([]).
ordered([_]).
ordered([H1, H2|T]) :-
	H1 =< H2,
	ordered([H2|T]),
	!.

list_length(List, L) :- list_length(List, 0, L).
list_length([], L, L).
list_length([_|T], Acc, L) :-
	Acc1 is Acc + 1,
	list_length(T, Acc1, L),!.

my_sum_list(List, S) :- my_sum_list_aux(List, 0, S).
my_sum_list_aux([], S, S).
my_sum_list_aux([H|T], Acc, S) :-
	Acc1 is Acc + H,
	my_sum_list(T, Acc1, S), 
	!.

rev_list_naive([], []).
rev_list_naive([H|T], Rev) :-
	rev_list_naive(T, RevT),
	append(RevT, [H], Rev),
	!.

rev_list([], []).
rev_list(List,  Rev) :- rev_list(List, [], Rev).
rev_list([], Acc, Acc).
rev_list([H|T], Acc, Rev) :-
	append([H], Acc, Acc1),
	rev_list(T, Acc1, Rev),
	!.

palindrome(List) :-
	rev_list(List, R),
	List = R,
	!.

fib(N, F) :- fib(N, 0, 1, F).
fib(0, _, F, F) :- !.
fib(N, F1, F2, F) :-
	N > 0,
	N1 is N -1,
	F3 is F1 + F2,
	fib(N1, F2, F3, F).

power(X, Y, Z) :- power(X, Y, 1, Z).
power(_, 0, Z, Z).
power(X, Y, Acc, Z) :-
	Y >= 0,
	Y1 is Y - 1,
	Acc1 is Acc * X,
	power(X, Y1, Acc1, Z).

combination(0, _, []).
combination(K, L, [H|T]) :-
	K > 0,
	K1 is K - 1,
	select(H, L, L1),
	combination(K1, L1, T).

edge(a, b).
edge(b, c).
edge(c, d).
edge(d, e).
edge(e, a).
edge(p, w).
path(X, Y) :- edge(X, Y), !.
path(X, Y) :- edge(X, Z), path(Z, Y).

% river crossing
change(l, r).
change(r, l).

move([X, X, S, C], wolf, [Y, Y, S, C]) :- change(X, Y).
move([X, W, X, C], sheep, [Y, W, Y, C]) :- change(X, Y).
move([X, W, S, X], cabbage, [Y, W, S, Y]) :- change(X, Y).
move([X, W, S, C], nothing, [Y, W, S, C]) :- change(X, Y).

safe([H, _, H, _]).
safe([H, H, _, H]).

solution([r, r, r, r], []).
solution(Config, [Move|Rest]) :-
	move(Config, Move, NextConfig),
	safe(NextConfig),
	solution(NextConfig, Rest).


nocheck(_, []).
nocheck(X/Y, [X1/Y1|Rest]) :-
	abs(Y1 - Y) =\= abs(X1 - X),
	% X =\= X1,
	Y =\= Y1,
	nocheck(X/Y, Rest).

legal([]).
legal([X/Y|Rest]) :-
	legal(Rest),
	% member(X, [1, 2, 3, 4, 5, 6, 7, 8]),
	member(Y, [1, 2, 3, 4, 5, 6, 7, 8]),
	nocheck(X/Y, Rest),
	!.

legal(_, []).
legal(N, [X/Y|Rest]) :-
	legal(N, Rest),
	between(1, N, Y),
	nocheck(X/Y, Rest).

solution_template(N, X) :- solution_template_aux(N, 0, X).
solution_template_aux(N, N, []) :- !.
solution_template_aux(N, Num, [H|T]) :-
	Num1 is Num + 1,
	H = Num1/_,
	solution_template_aux(N, Num1, T).

n_queens(N, X) :-
	solution_template(N, X),
	legal(N, X).
