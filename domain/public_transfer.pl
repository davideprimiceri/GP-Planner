/*
Find a path from one place to another by public transport.
Two operators: go_with_bus and go_with_metro.
It is possible to specify in the initial state a constraint on which transport to use.
*/

city_road(a,b,5).
city_road(a,e,8).
city_road(a,f,3).
city_road(b,c,6).
city_road(b,f,4).
city_road(b,g,2).
city_road(c,d,11).
city_road(c,k,8).
city_road(d,l,4).
city_road(d,n,5).
city_road(e,d,15).
city_road(e,f,2).
city_road(e,h,5).
city_road(f,g,2).
city_road(g,c,9).
city_road(h,i,7).
city_road(k,j,10).
city_road(n,m,2).

subway_road(a,b,2).
subway_road(a,f,2).
subway_road(b,d,4).
subway_road(b,g,1).
subway_road(c,m,2).
subway_road(d,l,1).
subway_road(d,m,3).
subway_road(e,f,1).
subway_road(f,g,1).
subway_road(f,b,1).
subway_road(g,c,2).
subway_road(g,d,4).

plan_operator(go_with_bus(X,Y),
    [at(X), use_transport(bus)],
    [at(Y)],
    [at(X)],
    Cost) :-
    city_road(X, Y, Cost).
	
plan_operator(go_with_metro(X,Y),
    [at(X), use_transport(metro)],
    [at(Y)],
    [at(X)],
    Cost) :- 
    subway_road(X, Y, Cost).