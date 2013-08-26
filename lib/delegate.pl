:- module(delegate,
        [   delegate/2,
            delegate/3,
            cut/1,
            delegate_and_test/3
        ]).
        
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(http/http_cookie)).


%%  delegate(+BaseURL, :Goal, +Options) is nondet
% 
%   Works just as call/1, but remotely, i.e. allows a program running on 
%   one Pengine server A to call and try to solve a possibly nondeterministic goal on another Pengine
%   server B, in the context of the program loaded on B. Of course, can also be used to call a Pengine 
%   server from any Prolog program. Options are passed to http_open/3.

delegate_and_test(BaseURL, Goal, Test) :-
    delegate(BaseURL, Goal),
    call(Test),
    cut(BaseURL),
    !.
    
delegate(BaseURL, Goal) :-
    delegate(BaseURL, Goal, []).

delegate(BaseURL, Goal, Options) :-
    catch(delegate_first(BaseURL, Goal, Options), Error, true),
    (   var(Error)
    ->  true
    ;   print_message(error, Error)
    ).


cut(BaseURL) :-    
    atomic_list_concat([BaseURL, '/prolog/abort'], URL),
    http_open(URL, _, []).

            
delegate_first(BaseURL, Goal, Options) :-
    term_to_atom(Goal, GoalAtom),
    atomic_list_concat([BaseURL, '/prolog/first?goal=', GoalAtom], URL),
    make_request(URL, BaseURL, Goal, Options).
    
            
delegate_next(BaseURL, Goal, Options) :-
    atomic_list_concat([BaseURL, '/prolog/next'], URL),
    make_request(URL, BaseURL, Goal, Options).
    
    
make_request(URL, BaseURL, Goal, Options) :-    
    http_open(URL, Stream, Options),
    json_read(Stream, Json),
    close(Stream),
    (   succeeded(Json, Goal, true)
    ;   succeeded(Json, Goal, false),
        !
    ;   failed(Json),
        !, fail
    ;   error(Json, Data),
        throw(Data),
        !
    ;   delegate_next(BaseURL, Goal, Options)
    ).


succeeded(json(List), Solution, More) :-
    memberchk(event=answer, List),
    memberchk(data=json(Data), List),
    memberchk(more= @More, Data),
    memberchk(solution=SolutionAtom, Data),
    atom_to_term(SolutionAtom, Solution, _).


failed(json(List)) :-
    memberchk(event=answer, List),
    memberchk(data=json(Data), List),
    memberchk(success= @false, Data).
    
    
error(json(List), Data) :-
    memberchk(event=error, List),
    memberchk(data=Data, List).



% delegate('http://localhost:5003', member(X, [1,2,3])), X = 2, cut('http://localhost:5003').
    
   