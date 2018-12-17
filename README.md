# GP-Planner
This project provides a Prolog implementation of the Graphplan algorithm proposed in:
>Blum, Avrim L., and Merrick L. Furst. "Fast planning through planning graph analysis." Artificial intelligence 90.1-2 (1997): 281-300.

Furthermore, an explanation of the plans obtained is provided in order to visualize the intermediate states of the planning process.

## Domain representation
The domains are defined according to the [STRIPS](https://en.wikipedia.org/wiki/STRIPS) representation, in this case it is also possible to specify the application cost of each action.

Two domains related to [rocket](domain/rocket.pl) scenario and [public transfers](domain/public_transfer.pl) are already defined.

Each domain is separated in a file containing the operations and in another file containing the initial state and the goal to be reached.

## How to use
The planner has been tested with SWI-Prolog 7.6.4 and YAProlog 6.2.2.

First of all, you need to load the main module:
```
consult('planner_start.pl').
```
Then calling the predicate start_planner will display a Text GUI to interact with the planner:
```
start_planner.
-----------------------------------------------------------------------
                        AUTOMATIC PLANNING MODULE                      
-----------------------------------------------------------------------


1. Load domain
2. Load initial & goal state
3. Plan
4. Explanation
0. Exit

|: 
```
You need to specify the path of the domain and the states for the planning:
```
|: 1.
Insert path of domain: |: 'domain/rocket'.

Domain loaded correctly

|: 2.
Insert path of states: |: 'domain/rocket_states'.

Initial state: at(a,london)  at(b,london)  at(rocket1,london)  has_fuel(rocket1)  
Goal: at(a,paris)  at(b,paris)
```
Then you can start the planning:
```
|: 3.


Plan found. Id: 1

Step 1:  load(rocket1,london,a)  load(rocket1,london,b)  
Step 2:  move(rocket1,london,paris)  
Step 3:  unload(rocket1,paris,a)  unload(rocket1,paris,b)  
Cost: 5
```
Explanations are provided by specifying the id of a returned plan:
```
|: 4.
Plan id: |: 1.


In the state: has_fuel(rocket1)  at(a,london)  at(b,london)  at(rocket1,london)  
I applied the actions: load(rocket1,london,a)  load(rocket1,london,b)  
to obtain the new state: has_fuel(rocket1)  at(rocket1,london)  in(a,rocket1)  in(b,rocket1)  

In the state: has_fuel(rocket1)  at(rocket1,london)  in(a,rocket1)  in(b,rocket1)  
I applied the action: move(rocket1,london,paris)  
to obtain the new state: at(rocket1,paris)  in(a,rocket1)  in(b,rocket1)  

In the state: at(rocket1,paris)  in(a,rocket1)  in(b,rocket1)  
I applied the actions: unload(rocket1,paris,a)  unload(rocket1,paris,b)  
to obtain the new state: at(a,paris)  at(b,paris)
```
Alternatively, the planner can be used without the Text GUI by:
  1. consulting a domain
  2. calling the predicate plan/3 specifying initial state and goal and returns plans in PlanSet 
  3. calling the predicate plan_explanation/1 specifying the id of the plan to be explained.

