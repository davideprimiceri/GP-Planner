/*
The rocket domain has 3 operators: load, unload, and move. 
A piece of cargo can be loaded into a rocket if the rocket and cargo are in the same location. 
A rocket may move if it has fuel.
*/

rocket(rocket1).
rocket(rocket2).
place(london).
place(paris).
place(rome).
cargo(a).
cargo(b).
cargo(c).
cargo(d).

plan_operator(move(Rocket,From,To),
	[at(Rocket,From), has_fuel(Rocket)],
	[at(Rocket, To)],
	[at(Rocket,From), has_fuel(Rocket)],
	1) :-
	rocket(Rocket),
	place(From),
	place(To),
	From \= To.

plan_operator(unload(Rocket, Place, Cargo),
	[at(Rocket,Place),in(Cargo,Rocket)], 
	[at(Cargo,Place)],
	[in(Cargo,Rocket)],
	1) :-
	rocket(Rocket),
	cargo(Cargo),
	place(Place).

plan_operator(load(Rocket, Place, Cargo),
	[at(Rocket,Place),at(Cargo,Place)],
	[in(Cargo,Rocket)],
	[at(Cargo,Place)],
	1) :-
	rocket(Rocket),
	cargo(Cargo),
	place(Place).