ntower(0, [], counts([], [], [], [])) :- !.
ntower(N, Rows, counts(C0, C1, C2, C3)) :-
	length(Rows, N),
	length(C0, N),
	maplist(consistent_row(N), Rows, C2, C3),
	transpose(Rows, Cols),
	maplist(consistent_row(N), Cols, C0, C1).

consistent_row(N, Row, C1, C2):-
	bijective_row(N, Row),
	visible_count(Row, C1),
	reverse(Row, RevRow),
	visible_count(RevRow, C2).

bijective_row(N, Row) :-
	length(Row, N),
	fd_domain(Row, 1, N),
	fd_all_different(Row), % redundant?
	fd_labeling(Row).

visible_count(Row, Clue) :- count_aux(Row, 0, 0, Clue).
count_aux([], _, C, C) :- !.
count_aux([H|T], Max, Count, Clue) :-
	Max1 is max(H, Max),
	(H > Max -> Count1 is Count + 1; Count1 = Count),
	% next_count(H, Max, Count, Count1),
	count_aux(T, Max1, Count1, Clue).

next_count(Curr, Max, C, C1) :-
	(Curr > Max, C1 is C + 1) ; (Curr < Max, C1 is C).


transpose(Rows, Cols) :- transpose(Rows, Cols, []).
transpose([[] | _], Cols, Acc) :-
	reverse(Cols, Acc).
transpose(Rows, Cols, Acc) :-
	split_col(Rows, Col, Rest),
	transpose(Rest, Cols, [Col|Acc]).

split_col([], [], []) :- !.
split_col([[H|T]|RowsRest], [H|ColRest], [T|RestRest]) :-
	split_col(RowsRest, ColRest, RestRest).

plain_ntower(0, [], counts([], [], [], [])) :- !.
plain_ntower(N, Rows, counts(C0, C1, C2, C3)) :-
	length(Rows, N),
	length(C0, N),
	maplist(consistent_row(N, plain), Rows, C2, C3),
	transpose(Rows, Cols),
	maplist(consistent_row(N, plain), Cols, C0, C1).

consistent_row(N, plain, Row, C1, C2):-
	bijective_row(N, Row, plain),
	visible_count(Row, C1),
	reverse(Row, RevRow),
	visible_count(RevRow, C2).

bijective_row(N, Row, plain) :-
	length(Row, N),
	maplist(between(1, N), Row),
	all_diff(Row).

all_diff([]) :- !.
all_diff([H|T]) :-
	\+(member(H, T)),
	all_diff(T).

testcase(N, C) :-
	N = 5,
	C = counts([1, 4, 3, 2, 2],
		   [3, 2, 3, 1, 3],
		   [1, 2, 3, 2, 3],
		   [3, 1, 2, 4, 2]).

speedup(Ratio) :-
	testcase(N, C),
	statistics(system_time, [_,_]),
	ntower(N, T1, C),
	statistics(system_time, [_,R1]),
	plain_ntower(N, T2, C),
	statistics(system_time, [_,R2]),
	Ratio is R2 / R1.
