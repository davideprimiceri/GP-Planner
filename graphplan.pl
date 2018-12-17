/* 
Graphplan algorithm implementation
*/

:- [planning_graph].
:- [planner_utility].

graphplan(Level, InitialState, _, _) :-
	planning_graph(Level, InitialState),
	fail.

graphplan(Level, InitialState, GoalState, PlanSet) :-
	check_predicates(Level, GoalState),
	not_mutex_predicates_set(Level, GoalState),
	assert(plan_id(0)),
	setof([Plan,Cost,PlanId], find_plan(Level, InitialState, GoalState, [], Plan, Cost, PlanId), PlanSet),
	retractall(plan_id(_)),
	!.
	
graphplan(Level, _, _, _) :-
	Level > 0,
	PreviousLevel is Level-1,
	findall(Predicate, state_level(Level, Predicate, _, add), State),
	findall(Predicate1, state_level(PreviousLevel, Predicate1, _, add), PreviousState),
	State == PreviousState,
	!,
	fail.
	
graphplan(Level, InitialState, GoalState, PlanSet) :-
	NextLevel is Level+1,
	%write('Livello:'), write('--'), write(NextLevel),nl,
	graphplan(NextLevel, InitialState, GoalState, PlanSet).

find_plan(0, InitialState, GoalState, Plan, Plan, 0, NextPlanId) :-
	p_subset(GoalState, InitialState),
	plan_id(PlanId),!,
	NextPlanId is PlanId + 1,
	asserta(plan_id(NextPlanId)).

find_plan(Level, InitialState, GoalState, PartialPlan, Plan, Cost, PlanId) :-
	Level > 0,
	PreviousLevel is Level-1,
	goal_actions(Level, GoalState, ActionSet),
	new_goal(PreviousLevel, ActionSet, NewGoal),
	remove_no_op_actions(ActionSet, NewActionSet),
	list_empty(NewActionSet, false),
	actions_cost(NewActionSet, ActionSetCost),
	find_plan(PreviousLevel, InitialState, NewGoal, [NewActionSet|PartialPlan], Plan, C1, PlanId),
	Cost is ActionSetCost + C1,
	assert(plan_explanation(PlanId, NewGoal, NewActionSet, GoalState)).

goal_actions(Level, GoalState, ActionSet) :-
	setof(Actions, goal_actions(Level, GoalState, [], Actions), Set),
	p_member(ActionSet, Set).
	
goal_actions(Level, [], ActionList, ActionSet) :-
	PreviousLevel is Level-1,
	sort(ActionList, ActionSet),
	not_mutex_actions_set(PreviousLevel, ActionSet).

goal_actions(Level, GoalState, ActionList, ActionSet) :-
	p_member(Predicate, GoalState),
	state_level(Level, Predicate, Action, add),
	setof(Pred, state_level(Level, Pred, Action, add), AddList),
	p_subtract(GoalState, AddList, NewGoalState),
	goal_actions(Level, NewGoalState, [Action|ActionList], ActionSet).
	
actions_cost([], 0).

actions_cost([Action|T], ActionSetCost) :-
	plan_operator(Action, _, _, _, Cost),
	actions_cost(T, C1),
	ActionSetCost is Cost + C1.

new_goal(Level, ActionSet, NewGoal) :-
	findall(Predicate, (p_member(Action, ActionSet), action_level(Level, Action, Predicate)), Preconditions),
	sort(Preconditions, NewGoal).