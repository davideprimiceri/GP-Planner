/*
Useful predicates
*/

p_member(X, [X|_]).        
p_member(X, [_|Tail]) :- 
	p_member(X, Tail).

p_subset([],_).
p_subset([H|T], Set) :-
	p_member(H, Set),
	p_subset(T, Set).
	
p_subtract([], _, []).
p_subtract([X|T], L, NewList) :- 
	p_member(X, L), 
	!, 
	p_subtract(T, L, NewList). 
p_subtract([X|T], L, [X|NewList]) :- 
	p_subtract(T, L, NewList).
	
list_empty([], true).
list_empty([_|_], false).

list_length([], 0).
list_length([_|T], L) :- 
	list_length(T, N), 
	L is N+1.

remove_no_op_actions([], []).
remove_no_op_actions([no_op(_)|T], L) :-
	!,
	remove_no_op_actions(T, L).
remove_no_op_actions([H|T], [H|L]) :-
	remove_no_op_actions(T, L).