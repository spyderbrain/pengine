:- module(family, 
    [   grandparent/2,
        grandfather/2,
        grandmother/2,
        parent/2,
        brother/2,
        sister/2,
        uncle/2,
        aunt/2,
        cousin/2,
        husband/2,
        wife/2,
        father/2,
        mother/2,
        male/1,
        female/1
    ]).

grandparent(X, Z) :-
    (   grandfather(X, Z)
    ;   grandmother(X, Z)
    ).

grandfather(X, Z) :-
    father(X, Y), 
    parent(Y, Z).

grandmother(X, Z) :-
    mother(X, Y),
    parent(Y, Z).

parent(X, Y) :-
    (   father(X, Y)
    ;   mother(X, Y)
    ).

brother(X, Y) :- 
    male(X), 
    parent(Z, X),
    parent(Z, Y), 
    X \== Y.

sister(X, Y) :-
    female(X), 
    parent(Z, X), 
    parent(Z, Y),
    X \== Y.

uncle(X, Y) :-
    male(X),
    parent(Z, X),
    grandparent(Z, Y).

aunt(X, Y) :-
    female(X),
    parent(Z, X),
    grandparent(Z, Y).

cousin(X, Y) :-
    parent(Z, X),
    parent(T, Y),
    (   brother(Z, T)
    ;   sister(Z, T)
    ).
    
husband(X, Y) :-
    father(X, Z),
    mother(Y, Z).
    
wife(X, Y) :-
    husband(Y, X).

father(fred, henry).
father(fred, bill).
father(fred, barb).
father(henry, cathy).

mother(sue, henry).
mother(sue, bill).
mother(sue, barb).
mother(barb, bob).

male(fred).
male(bob).
male(henry).
male(bill).

female(barb).
female(sue).
female(cathy).


    
