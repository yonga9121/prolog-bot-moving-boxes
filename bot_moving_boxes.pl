
unsafe(state(Y,_,X,X)) :- opp_room(X,Y).

move(state(Y,F,Y,Y), state(Y,B,Y,Y)) :- 
  	opp_greifer(F,B),
    not(unsafe(state(Y,B,Y,Y))).
move(state(Y,F,Y,X), state(Y,B,Y,X)) :- 
  	opp_greifer(F,B),
    not(unsafe(state(Y,B,Y,X))).
move(state(Y,F,X,Y), state(Y,B,X,Y)) :- 
  	opp_greifer(F,B),
    not(unsafe(state(Y,B,X,Y))).


move(state(X,busy,X,X), state(Y,busy,Y,X)) :- 
  	opp_room(X,Y),
    not(unsafe(state(Y,busy,Y,X))).
move(state(X,busy,X,X), state(Y,busy,X,Y)) :- 
  	opp_room(X,Y),
    not(unsafe(state(Y,busy,X,Y))).

move(state(X,busy,X,Y), state(Y,busy,Y,Y)) :- 
  	opp_room(X,Y),
    not(unsafe(state(Y,busy,Y,Y))).
move(state(X,busy,Y,X), state(Y,busy,Y,Y)) :- 
  	opp_room(X,Y),
    not(unsafe(state(Y,busy,Y,Y))).  

move(state(X,free,X,Y), state(Y,free,X,Y)) :- 
  	opp_room(X,Y),
    not(unsafe(state(Y,free,X,Y))).
move(state(X,free,Y,X), state(Y,free,Y,X)) :- 
  	opp_room(X,Y),
    not(unsafe(state(Y,free,Y,X))).  


opp_room(h1, h2).
opp_room(h2, h1).
opp_greifer(free, busy).
opp_greifer(busy, free).

empty_set([ ]).
member_set(E, S) :-
 	member(E, S).
delete_if_in_set(E, [E | T], T) :- !.
delete_if_in_set(E, [H | T], [H | T_new]) :- delete_if_in_set(E, T, T_new), !.
add_if_not_in_set(X, S, S) :- member(X, S), !.
add_if_not_in_set(X, S, [X | S]).
union([ ], S, S).
union([H | T], S, S_new) :- union(T, S, S2), add_if_not_in_set(H, S2, S_new),!.
subset([ ], _).
subset([H | T], S) :- member_set(H, S), subset(T, S).
intersection([ ], _, [ ]).
intersection([H | T], S, [H | S_new]) :- member_set(H, S), intersection(T, S, S_new), !.
intersection([_ | T], S, S_new) :- intersection(T, S, S_new), !.
set_difference([ ], _, [ ]).
set_difference([H | T], S, T_new) :- member_set(H, S), set_difference(T, S, T_new), !.
set_difference([H | T], S, [H | T_new]) :- set_difference(T, S, T_new), !.
equal_set(S1, S2) :- subset(S1, S2), subset(S2, S1).


empty_pq([ ]).
insert_pq(State, [ ], [State]) :- !.
insert_pq(State, [H | Tail], [State, H | Tail]) :- 
    precedes(State, H).
insert_pq(State, [H | T], [H | Tnew]) :-
    insert_pq(State, T, Tnew).
dequeue(E, [E | T], T).
member_pq(Element, Queue) :-
    member(Element, Queue).
precedes([_, _, _, NewH1, _], [_, _, _, NewH2, _]) :- NewH1 < NewH2.
insert_list_pq([ ], L, L).
insert_list_pq([State | Tail], L, New_L) :-
 	insert_pq(State, L, L2),
 	insert_list_pq(Tail, L2, New_L).
printsolution([State, nil, _, _, _], _) :-
    write(State), nl.
printsolution([State, Parent, _, _, _], Closed_set):-
    member_set([Parent, Grandparent, _, _, _],
    Closed_set),
    printsolution([Parent, Grandparent, _, _, _],
    Closed_set),
    write(State), nl.
path(Open_pq, _,_) :-
	empty_pq(Open_pq), 
    write("Graph searched, no solution found.").
path(Open_pq, Closed_set, Goal) :-
    dequeue([State, Parent, _, _, _], Open_pq,_),
    State = Goal,
    write("The solution path is: "), nl,
    printsolution([State, Parent, _, _, _],
    Closed_set).
path(Open_pq, Closed_set, Goal) :-
    dequeue([State, Parent, D, H, S], Open_pq,
    Rest_open_pq),
    get_children([State, Parent, D, H, S],
    Rest_open_pq, Closed_set, Children, Goal),
    insert_list_pq(Children, Rest_open_pq,
    New_open_pq),
    union([[State, Parent, D, H, S]], Closed_set,
    New_closed_set),
    path(New_open_pq, New_closed_set, Goal), !.
get_children([State,_,D,_, _], Rest_open_pq, Closed_set,Children,Goal) :-
    bagof(Child, moves([State, _, D, _, _], Rest_open_pq, Closed_set, Child,Goal), Children).
moves([State, _, Depth, _, _], Rest_open_pq, Closed_set,[Next,State,New_D,H,S], Goal) :-
    move(State, Next),
    not(unsafe(Next)), 
    not(member_pq([Next, _, _, _, _],Rest_open_pq)),
    not(member_set([Next, _, _, _, _],Closed_set)),
    New_D is Depth + 1,
    heuristic(Next, Goal, H),
    S is New_D + H.

heuristic(state(h1,free,h1,h1), state(h2,free,h2,h2), H) :- H is 8.

heuristic(state(h1,busy,h1,h1), state(h2,free,h2,h2), H) :- H is 7.

heuristic(state(h2,busy,h2,h1), state(h2,free,h2,h2), H) :- H is 5.

heuristic(state(h2,busy,h1,h2), state(h2,free,h2,h2), H) :- H is 5.

heuristic(state(h2,free,h2,h1), state(h2,free,h2,h2), H) :- H is 4.

heuristic(state(h2,free,h1,h2), state(h2,free,h2,h2), H) :- H is 4.

heuristic(state(h1,free,h2,h1), state(h2,free,h2,h2), H) :- H is 3.

heuristic(state(h1,free,h1,h2), state(h2,free,h2,h2), H) :- H is 3.

heuristic(state(h1,busy,h2,h1), state(h2,free,h2,h2), H) :- H is 2.

heuristic(state(h1,busy,h1,h2), sta/te(h2,free,h2,h2), H) :- H is 2.

heuristic(state(h2,busy,h2,h2), state(h2,free,h2,h2), H) :- H is 1.

heuristic(state(h2,free,h2,h2), state(h2,free,h2,h2), H) :- H is 0.


go(Start, Goal) :-
    empty_set(Closed_set),
    empty_pq(Open),
    heuristic(Start, Goal, H),
    insert_pq([Start, nil, 0, H, H], Open, Open_pq),
    path(Open_pq, Closed_set, Goal).


