/*
Rules for the planning graph
*/

%state_level(Level, Predicate, Action, Type)
:- dynamic state_level/4.

%action_level(Level, Action, Predicate)
:- dynamic action_level/3.

%mutex_predicates(Level, Predicate1, Predicate2)
:- dynamic mutex_predicates/3.

%mutex_actions(Level, Action1, Action2)
:- dynamic mutex_actions/3.

planning_graph(0, InitialState) :-
	add_level(0, _, InitialState, add, state).
	
planning_graph(Level, _) :-
	Level > 0,
	PreviousLevel is Level-1,
	applicable_action(PreviousLevel, Action, Preconditions, AddList, DeleteList),
	add_level(PreviousLevel, Action, Preconditions, _, action),
	add_level(Level, Action, AddList, add, state),
	add_level(Level, Action, DeleteList, del, state).

planning_graph(Level, _) :-
	Level > 0,
	PreviousLevel is Level-1,
	mutex_actions(PreviousLevel),
	mutex_predicates(Level).

add_level(_, _, [], _, _).

add_level(Level, Action, [H|T], Type, state) :-
	add_state_level(Level, H, Action, Type),
	add_level(Level, Action, T, Type, state).

add_level(Level, Action, [H|T], _, action) :-
	add_action_level(Level, Action, H),
	add_level(Level, Action, T, _, action).
	
add_state_level(Level, Predicate, Action, Type) :-
	state_level(Level, Predicate, Action, Type),
	!.
	
add_state_level(Level, Predicate, Action, Type) :-
	assert(state_level(Level, Predicate, Action, Type)).
		
add_action_level(Level, Action, Predicate) :-
	action_level(Level, Action, Predicate),
	!.
	
add_action_level(Level, Action, Predicate) :-
	assert(action_level(Level, Action, Predicate)).
	
applicable_action(Level, Action, Preconditions, AddList, DeleteList) :-
	plan_operator(Action, Preconditions, AddList, DeleteList, _),
	check_predicates(Level, Preconditions),
	not_mutex_predicates_set(Level, Preconditions).

applicable_action(Level, no_op(Predicate), [Predicate], [Predicate], []) :-
	state_level(Level, Predicate, _, add). %dal grafo recupero i predicati dell'add list
	
check_predicates(_, []).

check_predicates(Level, [Predicate|T]) :-
	state_level(Level, Predicate, _, add),
	check_predicates(Level, T),
	!.
	
not_mutex_predicates_set(_, [_]) :- 
	!.

not_mutex_predicates_set(Level, PredSet) :-
	setof( 
		(Predicate1, Predicate2),
		(p_member(Predicate1, PredSet), p_member(Predicate2, PredSet), Predicate1 \= Predicate2),
		PairSet
		),
	no_mutex(Level, PairSet, predicates).

not_mutex_actions_set(_, [_]) :-
	!.

not_mutex_actions_set(Level, ActionSet) :-
	setof( 
		(Action1, Action2),
		(p_member(Action1, ActionSet), p_member(Action2, ActionSet), Action1 \= Action2), 
		PairSet
		),
	no_mutex(Level, PairSet, actions).
	
no_mutex(_, [], _).
	
no_mutex(Level, [(Predicate1, Predicate2)|T], predicates) :-
	not(mutex_predicates(Level, Predicate1, Predicate2)),
	no_mutex(Level, T, predicates).

no_mutex(Level, [(Action1, Action2)|T], actions) :-
	not(mutex_actions(Level, Action1, Action2)),
	no_mutex(Level, T, actions).

mutex_actions(Level) :-	
	NextLevel is Level+1,
	state_level(NextLevel, Predicate, Action1, del),
	(
		state_level(NextLevel, Predicate, Action2, add);
		action_level(Level, Action2, Predicate)
	),
	Action1 \= Action2,
	add_mutex(Level, Action1, Action2, actions).

mutex_actions(Level) :-
	mutex_predicates(Level, Predicate1, Predicate2),
	action_level(Level, Action1, Predicate1),
	action_level(Level, Action2, Predicate2),
	Action1 \= Action2,
	add_mutex(Level, Action1, Action2, actions).
	
mutex_predicates(Level) :-
	PreviousLevel is Level-1,
	mutex_actions(PreviousLevel, Action1, Action2),
	state_level(Level, Predicate1, Action1, add),
	state_level(Level, Predicate2, Action2, add),
	Predicate1 \= Predicate2,
	not(mutex_predicates(Level, Predicate1, Predicate2)),
	not((
		state_level(Level, Predicate1, A1, add),
		state_level(Level, Predicate2, A2, add),
		not(mutex_actions(PreviousLevel, A1, A2))
		)),
	add_mutex(Level, Predicate1, Predicate2, predicates).

add_mutex(Level, Action1, Action2, actions) :-
	not(mutex_actions(Level, Action1, Action2)),
	assert(mutex_actions(Level, Action1, Action2)),
	assert(mutex_actions(Level, Action2, Action1)).
	
add_mutex(Level, Predicate1, Predicate2, predicates) :-
	not(mutex_predicates(Level, Predicate1, Predicate2)),
	assert(mutex_predicates(Level, Predicate1, Predicate2)),
	assert(mutex_predicates(Level, Predicate2, Predicate1)).