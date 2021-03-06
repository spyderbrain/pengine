:- module(queens, 
        [   queens/2
        ]).

%%   nqueens(+N, -Queens) is nondet.
%
%    @param    Queens is a list of column numbers for placing the queens.
%    @author Richard A. O'Keefe (The Craft of Prolog)


queens(N, Queens) :-
    length(Queens, N),
    board(Queens, Board, 0, N, _, _),
    nqueens(Board, 0, Queens).

board([], [], N, N, _, _).
board([_|Queens], [Col-Vars|Board], Col0, N, [_|VR], VC) :-
    Col is Col0+1,
    functor(Vars, f, N),
    constraints(N, Vars, VR, VC),
    board(Queens, Board, Col, N, VR, [_|VC]).

constraints(0, _, _, _) :- !.
constraints(N, Row, [R|Rs], [C|Cs]) :-
    arg(N, Row, R-C),
    M is N-1,
    constraints(M, Row, Rs, Cs).

nqueens([], _, []).
nqueens([C|Cs], Row0, [Col|Solution]) :-
    Row is Row0+1,
    select(Col-Vars, [C|Cs], Board),
    arg(Row, Vars, Row-Row),
    nqueens(Board, Row, Solution).
    
    