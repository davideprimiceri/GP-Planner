/*
Main module
*/

:- [graphplan].

start_planner :-
	planner(heading),
	menu_planner.
	
menu_planner :-
	planner(menu),
	nl,read(C),
	menu(C).
	
planner(heading) :- 
    nl,
    write('-----------------------------------------------------------------------'),nl,
    write('                        AUTOMATIC PLANNING MODULE                      '),nl,
    write('-----------------------------------------------------------------------'),nl.
	
planner(menu) :-
	nl,nl,
	write('1. Load domain'),nl,
	write('2. Load initial & goal state'),nl,
	write('3. Plan'),nl,
	write('4. Explanation'),nl,
	write('0. Exit'),nl.

menu(1) :-
	abolish(plan_operator/5),
	write('Insert path of domain: '),
	read(Path),
	consult(Path),
	current_predicate(plan_operator/5),
	nl,write('Domain loaded correctly'),
	menu_planner.

menu(2) :-
	abolish(plan_states/2),
	write('Insert path of states: '),
	read(Path),
	consult(Path),
	current_predicate(plan_states/2),
	plan_states(InitialState, GoalState),
	nl,write('Initial state: '),write_list(InitialState),
	nl,write('Goal: '),write_list(GoalState),
	menu_planner.

menu(3) :-
	current_predicate(plan_states/2),
	plan_states(InitialState, GoalState),
	plan(InitialState, GoalState, PlanSet),
	write_plan_set(PlanSet),
	menu_planner.
	
menu(4) :-
	write('Plan id: '),
	read(PlanId),
	plan_explanation(PlanId).

menu(4) :-
	menu_planner.
	
menu(0) :-
	!.

plan(InitialState, GoalState, PlanSet) :-
	retractall(state_level(_, _, _, _)),
	retractall(action_level(_, _, _)),
	retractall(mutex_actions(_, _, _)),
	retractall(mutex_predicates(_, _, _)),
	retractall(plan_explanation(_, _, _, _)),
	retractall(plan_goal(_)),
	assert(plan_goal(GoalState)),
	graphplan(0, InitialState, GoalState, PlanSetNotOrdered),
	plans_ordered(PlanSetNotOrdered, PlanSet).
	
plans_ordered(PlanSet, OrderedPlanSet) :-
	costs_ordered(PlanSet, [], OrderedCost),
	findall([Plan,Cost,PlanId], (p_member(Cost, OrderedCost), p_member([Plan,Cost,PlanId],PlanSet)), OrderedPlanSet).

costs_ordered([], Cost, OrderedCosts) :-
	sort(Cost, OrderedCosts).

costs_ordered([[_,Cost,_]|T], C, OrderedCosts) :-
	costs_ordered(T, [Cost|C], OrderedCosts).
	
write_plan_set([]).
write_plan_set([[Plan,Cost,PlanId]|T]) :-
	nl,nl,write('Plan found. Id: '),write(PlanId),nl,nl,
	write_plan_steps(1, Plan),
	write('Cost: '),write(Cost),
	write_plan_set(T).
	
write_plan_steps(_, []).
write_plan_steps(N, [Step|T]) :-
	write('Step '),write(N),write(':  '),write_list(Step),nl,
	Next is N+1,
	write_plan_steps(Next, T).
	
write_list([]).
write_list([H|T]) :-
	write(H),write('  '),
	write_list(T).
	
plan_explanation(PlanId) :-
    plan_goal(GoalState),
    plan_explanation(PlanId, State, Actions, NewState),
    nl,nl,write('In the state: '), write_list(State),nl, 
    write('I applied '),
    (
    (list_length(Actions, 1), write('the action: '));
    (not(list_length(Actions, 1)), write('the actions: '))
    ),
    write_list(Actions),nl, 
    write('to obtain the new state: '), write_list(NewState),
    retract(plan_explanation(PlanId, State, Actions, NewState)),
    NewState == GoalState, !, 
    plan_explanation(PlanId).