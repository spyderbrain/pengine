:- module(consult, []).

% http library modules 
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_session)).
:- use_module(library(memfile)).
:- use_module(library(debug)).

:- use_module(library(sandbox)).

:- use_module(lib/http_cors).

:- style_check(-atom).


:- http_handler(root(prolog/consult), consult, []).
:- http_handler(root(prolog/listing), do_listing, []).

% TBD: Check if load_files/2 and the option sandboxed_load(bool) can be used here instead.

consult(Request) :-
    http_session_id(SessionId),
    (   thread_property(SID, status(running)),
        SessionId == SID
    ->  catch(thread_signal(SessionId, abort), _, true),
        sleep(0.5)
    ;   true
    ),
    catch(consult_1(Request), Error, true),
    (   var(Error)
    ->  true
    ;   message_to_string(Error, Msg),
        cors_enable, reply_json(json([event=error, data=Msg]), [width(0)])
    ).




consult_1(Request) :-
    setup_call_cleanup(new_memory_file(Handle),
        consult_code(Request, Handle),
        free_memory_file(Handle)
    ).


%%    consult_code(+Request, +MemFile)

consult_code(Request, Handle) :-
    setup_call_cleanup(open_memory_file(Handle, write, Output),
        http_read_data(Request, _, [to(stream(Output))]),
        close(Output)),
    setup_call_cleanup(open_memory_file(Handle, read, Stream),
        consult_stream(Stream),
        close(Stream)),
    cors_enable, reply_json(json([event=loaded]), [width(0)]).


consult_stream(Stream) :-
    http_session_id(SessionId),
    forall(current_predicate(SessionId:PI),
        (   memberchk(PI, [read/1, write/1, writeln/1, nl/0])
        ->  true
        ;   abolish(SessionId:PI)
        )
    ),
    repeat,
        read_term(Stream, Term, [module(SessionId)]),
        (   Term == end_of_file
        ->  !
        ;   expand_term(Term, ExpandedTerm),
            consult_term(ExpandedTerm, SessionId),
            fail
        ).


consult_term(Var, _) :-
    var(Var), !,
    instantiation_error(Var).
consult_term([], _) :- !.
consult_term([H|T], Module) :- !,
    consult_term(H, Module),
    consult_term(T, Module).
consult_term((:- Directive), Module) :- !,
    expand_goal(Directive, Goal),
    (   safe_goal(Module:Goal)
    ->  Module:Goal
    ;   permission_error(execute, goal, Goal)
    ).
consult_term(Clause, Module) :-
    not_qualified(Clause),
    assert(Module:Clause).

not_qualified(Clause) :-
    Clause = _:_, !,
    permission_error(assert, clause, Clause).
not_qualified(_).


%%   do_listing(+Request) is det.
%
%    HTTP handler for listing the clauses in the SessionID module.

do_listing(_Request) :-
    http_session_id(SessionId),
    findall(PI, (current_predicate(SessionId:PI), \+ member(PI, [read/1, write/1, writeln/1, nl/0])), PIs),
    phrase(listing(PIs, SessionId), Codes),
    atom_codes(Listing, Codes),
    cors_enable, reply_json(json([event=listed, data=Listing])). 

listing([], _SessionId) --> [].    
listing([PI|PIs], SessionId) -->
   listing(PI, SessionId),
   listing(PIs, SessionId).

listing(PI, SessionId, In, Tail) :-
        with_output_to(codes(In, Tail), SessionId:listing(PI)).

