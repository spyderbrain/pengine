:- module(prolog_server,
      [ server/1,            % ?Port
        input/1,
        input/2,
        set_prompt/1,
        output/1
      ]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_server_files)).
:- use_module(library(debug)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(broadcast)).
:- use_module(library(time)).
:- use_module(library(settings)).
:- use_module(library(sandbox)).
:- use_module(library(http/http_authenticate)).
%:- use_module(library(resource_bounds)).

:- use_module(lib/term_to_json).
:- use_module(lib/http_cors).



:- setting(call_timeout, number, 15, 'Call timeout in seconds').
:- setting(session_timeout, number, 600, 'Session timeout in seconds').

:- listen(settings(changed(Module:session_timeout, _Old, New)), http_set_session_options([timeout(New)])), context_module(Module).

:- debug(pengine).


/** <module> Prolog Web Server

This module is a demonstration of   handling  a conversation with Prolog
through a web-interface where state for the   conversation  is kept in a
non-deterministic Prolog predicate.

The design associates a thread that runs the conversation (and keeps the
state) with each HTTP session. This thread is created by first/1 and can
end due to next/1, stop/1  or  timeout   of  the  session. The latter is
signalled through the library(broadcast).  See   the  directive  calling
listen/2.

@author    Torbjörn Lager
@author    Jan Wielemaker

*/

:- http_handler(root(tutorial), http_reply_file('www/tutorial.html', []), [prefix]).
:- http_handler(root(admin/apps), http_reply_file('www/admin/application.html', []), [prefix, authentication(basic(passwd, admin))]).
:- http_handler(root(admin/server), http_reply_file('www/admin/server.html', []), [prefix, authentication(basic(passwd, admin))]).
:- http_handler(root(admin/statistics), http_reply_file('www/admin/statistics.html', []), [prefix, authentication(basic(passwd, admin))]).
:- http_handler(root(admin/account), http_reply_file('www/admin/account.html', []), [prefix, authentication(basic(passwd, admin))]).



:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

:- prolog_load_context(directory, Dir),
   asserta(user:file_search_path(app, Dir)).

user:file_search_path(www, app(www)).
user:file_search_path(apps, app(apps)).
user:file_search_path(app_plugins, app(plugins)).
user:file_search_path(examples, app(examples)).


:- http_handler(root(apps), serve_files_in_directory(apps), [prefix]).
:- http_handler(root(.), serve_files_in_directory(www), [prefix]).

:- http_handler(root(examples), serve_files_in_directory(examples), [prefix]).

:- http_handler(root(admin), http_redirect(moved, root(admin/apps)), []).
:- http_handler(/, http_redirect(moved, root(admin/apps)), []).




%%    server(?Port) is det.
%
%    Start the web-server on Port.

server(Port) :-
    http_server(http_dispatch,
            [ port(Port),
              workers(16)
            ]),
    format('You can access the server at http://localhost:~w/~n', [Port]).





%   When a client session begins, an output queue is created
%   from which the client will have to fetch all output
%   generated in the process of solving a goal. Furthermore,
%   a module, named by the session ID and thus "private" to the 
%   client, is created and prepared.

:- listen(http_session(begin(SessionId, _Peer)), begin_session(SessionId)).
    
begin_session(SessionId) :-
    debug(pengine, 'Begin session: ~q', [SessionId]),
    atom_concat(SessionId, '.out', Output),
    atom_concat(SessionId, '.comm', Communication),
    message_queue_create(Output),
    message_queue_create(Communication),
    prepare_module(SessionId).    


%   When a client session ends, the currently running goal (if any)
%   is aborted, the output queue is destroyed, and the client module 
%   is emptied.

:- listen(http_session(end(SessionId, _Peer)), end_session(SessionId)).

end_session(SessionId) :-
    debug(pengine, 'End session: ~q', [SessionId]),
    catch(thread_signal(SessionId, abort), _, true),
    atom_concat(SessionId, '.out', Output),
    atom_concat(SessionId, '.comm', Communication),
    catch(message_queue_destroy(Output), _, true),
    catch(message_queue_destroy(Communication), _, true),
    forall(current_predicate(SessionId:PI),
        (   memberchk(PI, [read/1, write/1, writeln/1, nl/0])
        ->  true
        ;   abolish(SessionId:PI)
        )
    ).


%%  input_queue(-QueueName) is det.
%
%   The input queue is given the same name as the current session ID.

input_queue(Input) :-
    http_session_id(Input).


%%  output_queue(-QueueName) is det.
%
%   The name of the output queue is derived from the session ID too.
%   (Note that the session ID is also the thread id for goals that 
%   are running.)

output_queue(Output) :-
    http_session_id(SessionId),
    atom_concat(SessionId, '.out', Output).
    

%%  communication_queue(-QueueName) is det.
%
%   The name of the communication queue is derived from the session ID too.
%   (Note that the session ID is also the thread id for goals that are running.)

communication_queue(Communication) :-
    http_session_id(SessionId),
    atom_concat(SessionId, '.comm', Communication).

    
%%  prepare_module(+Module) is det.
%
%   Create and prepare a module by (re)defining some
%   I/O predicates in it.
%
%   @tbd Investigate if there are other and better ways to
%   redefine the I/O predicates.

prepare_module(Module) :-
    Module:redefine_system_predicate(read(_)),
    Module:redefine_system_predicate(write(_)),
    Module:redefine_system_predicate(writeln(_)),
    Module:redefine_system_predicate(nl),
    assert(Module:(read(Term) :- input(Term))),
    assert(Module:(write(Term) :- term_to_atom(Term, Atom), output(Atom))),
    assert(Module:(writeln(Term) :- term_to_atom(Term, Atom0), atom_concat(Atom0, '<br />', Atom), output(Atom))),
    assert(Module:(nl :- output('<br />'))).
    


%%%%%%%%%%%     HTTP handlers    %%%%%%%%%%%

%    Declare HTTP locations we serve and how.

:- http_handler(root(prolog/first), first, []).
:- http_handler(root(prolog/next),  next,  []).
:- http_handler(root(prolog/input),  input_read,  []).
:- http_handler(root(prolog/stop),  stop,  []).
:- http_handler(root(prolog/abort), abort, []).
:- http_handler(root(prolog/result), result, []).
        
                    

%%   first(+Request) is det.
%
%    HTTP handler that starts generating solutions for a query.

first(Request) :-
    http_parameters(Request,
            [ goal(GoalAtom, []),
              format(Format, [default(prolog)])
            ]),
    http_session_id(SessionId),
    set_output_format(Format),
    catch(text_to_goal(GoalAtom, Goal, Bindings), E, true),
    (   var(E)
    ->  output_queue(Output),
        empty_queue(Output),
        first(SessionId, Goal, Bindings),
        output_result
    ;   message_to_string(E, Msg),
        cors_enable, reply_json(json([event=error, data=Msg]))
    ).


%%  set_output_format(+Format) is det.
%

set_output_format(Format) :-
    http_session_retractall(output_format(_)),
    http_session_assert(output_format(Format)).

%%  get_output_format(-Format) is det.
%

get_output_format(Format) :-
    http_session_data(output_format(Format)).
    
    
    

first(SessionId, Goal, Bindings) :-
    catch(first_1(SessionId, Goal, Bindings), E, true),
    (   var(E)
    ->  true
    ;   debug(pengine, 'ERROR: ~p', [E]),
        catch(thread_signal(SessionId, abort), _, true),
        sleep(0.3),
        first(SessionId, Goal, Bindings)
    ).
    

first_1(SessionId, Goal, Bindings) :-
    thread_create(solve(SessionId:Goal, Bindings), _, 
            [detached(true), alias(SessionId)]).
    

:- multifile
	sandbox:safe_primitive/1,		% Goal
	sandbox:safe_meta/2.			% Goal, Calls

sandbox:safe_primitive(prolog_server:input(_)).
sandbox:safe_primitive(prolog_server:input(_, _)).
sandbox:safe_primitive(prolog_server:output(_)).
sandbox:safe_primitive(prolog_server:set_prompt(_)).

sandbox:safe_primitive(clpfd:nb_getval(_,_)).
sandbox:safe_primitive(M:write(_)) :- http_session_id(M).
sandbox:safe_primitive(M:writeln(_)) :- http_session_id(M).
sandbox:safe_primitive(M:read(_)) :- http_session_id(M).
sandbox:safe_primitive(M:nl) :- http_session_id(M).
sandbox:safe_primitive(system:sleep(_)). 
sandbox:safe_primitive(system:prompt(_,_)). 
sandbox:safe_primitive(dif:dif(_,_)).
sandbox:safe_primitive(clpfd:Pred) :- predicate_property(clpfd:Pred, exported).
sandbox:safe_primitive(atomic(_)).
sandbox:safe_primitive(sub_atom(_,_,_,_,_)).


%%  text_to_goal(+GoalAtom, -Goal, -Bindings) is det.
%
%   True if Goal is the term represention of GoalAtom and Bindings
%   the bindings as given by atom_to_term/3.
%   
%   Throws a syntax error if GoalAtom doesn't adhere to Prolog syntax,
%   or a permission error if safe_goal/1 says Goal is unsafe.
%
%   @tbd Find a way to distinguish permission errors from errors generated
%   for undefined predicates.

text_to_goal(GoalAtom, Goal, Bindings) :-
    http_session_id(SessionId),
    catch(
            (   atom_to_term(GoalAtom, Goal, Bindings0), %read_term_from_atom(GoalAtom, Goal, [variable_names(Bindings)]),
                filter_underscored(Bindings0, Bindings),
                (   pengine_permitted(Goal, _App)
                ->  true
                ;   safe_goal(SessionId:Goal)
                ->  true
                ;   permission_error(execute, goal, Goal)
                )
            ),
        Error,
        throw(Error)
    ).
    

%%  filter_underscored(+Bindings, -FilteredBindings) is det.
%
%   True if variable names in Bindings that start with an
%   underscore is no longer present in FilteredBindings.


filter_underscored([], []).
filter_underscored([F=_V|FVs0], FVs) :-
    atom_concat('_', _, F), !,
    filter_underscored(FVs0, FVs).    
filter_underscored([FV|FVs0], [FV|FVs]) :-
    filter_underscored(FVs0, FVs).
    


%%  empty_queue(+Queue) is det.
%
%   Empty the queue. Just to be sure we are not hit by 
%   stuff left from the processing of a previous goal.

empty_queue(Queue) :-
    thread_peek_message(Queue, _), !,
    thread_get_message(Queue, _),
    empty_queue(Queue).
empty_queue(_Queue).


%%  solve(:Goal, +Bindings) is det.
%
%   Generate for Goal and send results to the output queue. 
%   Starts a timer that will abort the current goal on timeout.
%   We run solve/2 in setup_call_cleanup/3 so that the timer
%   and associated data is properly setup and cleaned up
%   afterwards even if the goal is aborted.


solve(Goal, Bindings) :-
    setup_call_cleanup(set_timeout, solve_1(Goal, Bindings), clear_timeout).


%%  set_timeout is det.
%

set_timeout :-
    % Hmm, why is this part needed? Shouldn't
    % setup_call_cleanup/3 take care of it?
    (   http_session_data(alarmId(AlarmId0))
    ->  catch(remove_alarm(AlarmId0), _, true),
        http_session_retractall(alarmId(_))
    ;   true
    ),
    setting(call_timeout, Timeout),
    alarm(Timeout, (
            debug(pengine, 'ALARM!!!', []),
            input_queue(Input),
            output_queue(Output),
            thread_send_message(Output, result(error, 'Time limit exceeded', _, _, _)),
            sleep(0.3), % Hmm, can we avoid this?
            catch(thread_signal(Input, abort), _, true)
        ),
        AlarmId
    ),
    http_session_assert(alarmId(AlarmId)).



%%  clear_timeout is det.
%

clear_timeout :-
    http_session_data(alarmId(AlarmId)),
    catch(remove_alarm(AlarmId), _, true).


%%  solve_1(:Goal, +Bindings) is det.
%
%   Generate for Goal and send results to the output queue. 

solve_1(Goal, Bindings) :-
    thread_self(Me),
    thread_statistics(Me, cputime, T0a),
    nb_setval(time, T0a),
    solve_2(Goal, Bindings, Solution),
    nb_getval(time, T0),
    thread_statistics(Me, cputime, T1),
    Time is T1 - T0,
    result_time(Solution, Time),
    nb_setval(time, T1),
    debug(pengine, 'Sending: ~q', [Solution]),
    output_queue(Output),
    thread_send_message(Output, Solution),
    result_more_sol(Solution, More),
    (   More == false
    ->  true
    ;   More == true
    ->  thread_get_message(command(Command)),
        debug(pengine, 'Command: ~q', [Command]),
        Command == stop
    ;   true
    ).

result_more_sol( result(_, _, _, T, _), T).
result_time(     result(_, _, _, _, T), T).


%%   solve_2(:Goal, +Bindings, -Solution) is nondet.
%
%    Solve Goal. This predicate catches errors and detects whether
%    Goal succeeded deterministically.

solve_2(Goal, Bindings, Solution) :-
    call_cleanup(catch(Goal, Error, true), Det=true),
    (   var(Error)
    ->  (    Det == true
        ->   More = false
        ;    More = true
        ),
        Solution = result(true, Goal, Bindings, More, _)
    ;   message_to_string(Error, Msg),
        Solution = result(error, Msg, _, _, _)
    ).
solve_2(_Goal, _, result(false, _, _, _, _)).


%%    next(+Request) is det.
%
%    HTTP handler for the next answer.


next(_Request) :-
    input_queue(Input),
    catch(thread_send_message(Input, command(next)), E, true),
    (   var(E)
    ->  output_result
    ;   cors_enable, reply_json(json([event=error, data='Thread is not running']))
    ).


%%   input_read(+Request) is det.
%
%    HTTP handler for handling data client input (read/1).

input_read(Request) :-
    http_parameters(Request,
            [ input(TermAtom, [])
            ]),
    input_queue(Input),
    thread_send_message(Input, input(TermAtom)),
    output_result.


%%   stop(+Request) is det.
%
%    HTTP handler to stop the interaction.

stop(_Request) :-
    input_queue(Input),
    catch(thread_send_message(Input, command(stop)), _, true),
    cors_enable, reply_json(json([event=halted])).   


%%   abort(+Request) is det.
%
%    HTTP handler to abort the interaction.

abort(_Request) :-
    input_queue(Input),
    output_queue(Output),
    thread_send_message(Output, result(error, 'Execution Aborted', _, _, _)),
    sleep(0.3),    
    catch(thread_signal(Input, abort), _, true),
    cors_enable, reply_json(json([ok= @true])).
    

%%   input(-Term) is det.
%
%    

input(Term) :-
    input(Term, 0).


%%   input(-Term, Timeout) is det.
%
%    

input(Term, Timeout) :-
    input_queue(Input),
    output_queue(Output),
    input_prompt(CurrentPrompt),
    thread_send_message(Output, result(prompt, CurrentPrompt, _, _, _)),
    (Timeout > 0 -> Options = [timeout(Timeout)] ; Options = []),
    thread_get_message(Input, input(TermAtom), Options),
    atom_to_term(TermAtom, Term, _).


%%   set_prompt(+Term) is det.
%
%    

set_prompt(Prompt) :-
    http_session_retractall(current_prompt(_)),
    http_session_assert(current_prompt(Prompt)).
    

%%   input_prompt(-Term) is det.
%
%    

input_prompt(Prompt) :-
    (   http_session_data(current_prompt(Prompt))
    ->  true
    ;   Prompt = '|: '
    ).
    
    

%%   output(+Term) is det.
%
%    

output(Term0) :-
    output_queue(Output),
    communication_queue(Communication),
    term_to_json(Term0, Term),
    thread_send_message(Output, result(output, Term, _, _, _)),
    thread_get_message(Communication, ack).


    

%%   result(+Request) is det.
%
%    HTTP handler to collect (more) results.

result(_Request) :-
    output_result.
    

%%   output_result is det.
%
%    Wait for the goal thread to send a result and
%    call the output_result/5 predicate

output_result :-
    debug(pengine, 'Waiting for result ...', []),
    output_queue(Output),
    communication_queue(Communication),
    thread_get_message(Output, Msg),
    debug(pengine, 'Received: ~q', [Msg]),
    (   arg(1, Msg, output)
    ->  thread_send_message(Communication, ack)
    ;   true
    ),
    Msg = result(Success, Message, Bindings, More, Time),
    get_output_format(Format),
    (   Format == prolog
    ->  true
    ;   Format == json
    ->  output_json_result(Success, Message, Bindings, More, Time)
    ;   Format == 'json-s'
    ->  output_json_s_result(Success, Message, Bindings, More, Time)
    ).


%%   output_json_result(+Kind, +Message, +Bindings, +More, +Time) is det.
%
%    Convert result/5 terms into JSON.

output_json_result(true, _Goal0, Bindings0, More0, Time0) :-
%    strip_module(Goal0, _Module, Solution0),
%    term_to_atom(Solution0, Solution),
    term_to_json(Bindings0, Bindings),
    cors_enable, reply_json(json([event=answer, data=json([success= @true, bindings=Bindings, more= @More0, time=Time0])])).
output_json_result(false, _Module, _Bindings0, _More0, Time0) :-
    cors_enable, reply_json(json([event=answer, data=json([success= @false, time=Time0])])).
output_json_result(error, Message0, _Bindings0, _More0, _Time0) :-
    cors_enable, reply_json(json([event=error, data=Message0])).
output_json_result(halted, Message0, _Bindings0, _More0, _Time0) :-
    cors_enable, reply_json(json([event=halted, data=Message0])).
output_json_result(prompt, Term0, _Bindings0, _More0, _Time0) :-
    term_to_json(Term0, Json),
    cors_enable, reply_json(json([event=prompt, data=Json])).
output_json_result(output, Message0, _Bindings0, _More0, _Time0) :-
    cors_enable, reply_json(json([event=output, data=Message0])).



%%   output_json_s_result(+Kind, +Message, +Bindings, +More, +Time) is det.
%
%    Convert result/5 terms into simple JSON.

output_json_s_result(true, _Goal0, Bindings0, More0, Time0) :-
%    strip_module(Goal0, _Module, Solution0),
%    term_to_atom(Solution0, Solution),
    bindings_to_json(Bindings0, Bindings),
    cors_enable, reply_json(json([event=answer, data=json([success= @true, bindings=Bindings, more= @More0, time=Time0])])).
output_json_s_result(false, _Module, _Bindings0, _More0, Time0) :-
    cors_enable, reply_json(json([event=answer, data=json([success= @false, time=Time0])])).
output_json_s_result(error, Message0, _Bindings0, _More0, _Time0) :-
    cors_enable, reply_json(json([event=error, data=Message0])).
output_json_s_result(halted, Message0, _Bindings0, _More0, _Time0) :-
    cors_enable, reply_json(json([event=halted, data=Message0])).
output_json_s_result(prompt, Term0, _Bindings0, _More0, _Time0) :-
    term_to_json(Term0, Json),
    cors_enable, reply_json(json([event=prompt, data=Json])).
output_json_s_result(output, Message0, _Bindings0, _More0, _Time0) :-
    cors_enable, reply_json(json([event=output, data=Message0])).
    


bindings_to_json(BindingsIn, json(BindingsOut)) :-
    maplist(swap, BindingsIn, BindingsOut).

swap(N=V, N=A) :- term_to_atom(V, A).   




    

    
    