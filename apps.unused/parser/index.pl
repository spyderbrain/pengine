:- module(parser,
	  [ parse/3,
	    op(1200, xfy, (--->))
	  ]).

:- meta_predicate
	parse(:, +, -).

%%	parse(:Cat, +Atom, -ParseTree) is nondet.
%
%	A simple parser

parse(Cat, Atom, JsonTree) :-
	tokenize_atom(Atom, AtomList),
	parse(Cat, AtomList, [], JsonTree).

parse(M:A, P0, P, json([label=AA, children=Tree])) :-
	M:(A ---> B),
	parse_body(M:B, P0, P, Tree),
	term_to_atom(A, AA).
parse(_:[Word], [Word|P], P, json([label=Word])).

parse_body(M:(B,Bs), P0, P, [Tree|Trees]) :- !,
	parse(M:B, P0, P1, Tree),
	parse_body(M:Bs, P1, P, Trees).
parse_body(B, P0, P, [Tree]) :-
	parse(B, P0, P, Tree).
