:- module(bags,
      [ bag/4
      ]).

%%  bag(+N, +Template, :Goal, -Bag)
%   Create a list of the (up to) N instantiations Template gets 
%   successively on backtracking over Goal and unify 
%   the result with Bag. Succeds again with the next (up to) N 
%   instantiations of Template on backtracking. Fails is Goal
%   has no solutions.

bag(N, Template, Goal, Bag) :-
    copy_term(Template-Goal, TemplateCopy-GoalCopy),
    Counter = counter(0),
    (   call(GoalCopy),
        arg(1, Counter, N1),
        N2 is N1 + 1,
        nb_setarg(1, Counter, N2),
        recordz(sols, TemplateCopy),
        N2 == N,
        nb_setarg(1, Counter, 0)
    ;   true
    ),
    findall(Row, (recorded(sols, Row, Ref), erase(Ref)), Bag),
    Bag \= [].

    