% This system may only distributed using the GNU General Public License
% because the following components contain GPL-ed code:
% 
%     /opt/local/lib/swipl-6.3.15/library/mime.pl
%     GNU Readline library
% 
% See http://www.swi-prolog.org/license.html for details on
% SWI-Prolog licensing policies supporting both free and non-free
% Software.


:- module(applications, 
        [   pengine_permitted/2
        ]).


% http library modules 
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(settings)).
:- use_module(library(debug)).

:- use_module(lib/mysandbox).


:- http_handler(root(admin/apps/analyse), apps_analyse, []).
:- http_handler(root(admin/apps/list), apps_list, []).
:- http_handler(root(admin/app/load), app_load, []).
:- http_handler(root(admin/app/unload), app_unload, []).
:- http_handler(root(admin/app/block), app_block_pred, []).
:- http_handler(root(admin/app/unblock), app_unblock_pred, []).



%%	apps_analyse(+Request) is det.
%
%	Find  .pl module files  in  the apps directory, and register
%   them. Note that the modules are not actually loaded.


apps_analyse(_Request) :-
    retractall(app_to_file(_, _)),
    retractall(pred_to_app(_, _)),
	expand_file_name('apps/*/*.pl', Files),
	maplist(read_module, Files, Info),
	keysort(Info, SortedInfo),
	apps_register(SortedInfo),
	reply_json(json([ok= @true])).


:- dynamic app_to_file/2.

read_module(File, App-Predicates) :-
	setup_call_cleanup(open(File, read, Stream), 
	    read(Stream, ':-'(module(App, Predicates))), 
	    close(Stream)
	),
	assert(app_to_file(App, File)).



apps_register([]).
apps_register([App-PiList|T]) :-
	app_register(App, PiList),
	apps_register(T).


:- dynamic pred_to_app/2.
:- dynamic loaded/2.
:- dynamic safe/3.
:- dynamic blocked/3.


app_register(App, List) :-
    forall(member(PI, List), 
        (   assert(pred_to_app(PI, App)),
            (   blocked(App, PI, _)
            ->  true
            ;   assert(blocked(App, PI, false))
            )
        )
    ),
    (   loaded(App, _)
    ->  true
    ;   assert(loaded(App, false))
    ).



%%	apps_list(+Request) is det.
%
%	

apps_list(_Request) :-
    findall(json([app=App, file=File, loaded= @Bool, preds=Preds]),
        setof(json([pred=PIA, safety=Safety, blocked= @Blocked]), 
            PI^(
                loaded(App, Bool), 
                app_to_file(App, File), 
                pred_to_app(PI, App),
                (safe(App, PI, Safety) -> true ; Safety=unknown),
                blocked(App, PI, Blocked),
                term_to_atom(PI, PIA)
            ), 
        Preds),
    List),
    reply_json(json([apps=List])).



%%	app_load(+Request) is det.
%

app_load(Request) :-
    http_parameters(Request,
            [ app(App, [])
            ]),
    app_to_file(App, File),
    catch(user:load_files(File, [must_be_module(true), imports(all)]), Error, assert(error(Error))),
    (   retract(error(Error))
    ->  unload_file(File),
        message_to_string(Error, Msg),
        reply_json(json([app=App, loaded= @false, error=Msg]))
    ;   safety_analysis(App, List),
        retractall(loaded(App, _)),
        assert(loaded(App, true)),
        reply_json(json([app=App, loaded= @true, safety=List]))
    ).


:- multifile
	user:message_hook/3.
:- dynamic
	user:message_hook/3.
	
user:message_hook(Error, error, _Lines) :-
    assert(error(Error)).



safety_analysis(App, List) :-
    findall(json([pred=PIA, safety=Safety]), 
        (   pred_to_app(PI, App), 
            safe_pi(PI, Safety), 
            retractall(safe(App, PI, _)),
            assert(safe(App, PI, Safety)),
            determine_block(Safety, Bool),
            retractall(blocked(App, PI, _)),
            assert(blocked(App, PI, Bool)),
            term_to_atom(PI, PIA)
        ), 
        List).
    

safe_pi(F/N, Safety) :-
    functor(G, F, N),
    (   catch(mysafe_goal(G), Error, true),
        var(Error)
    ->  Safety = safe
    ;   Safety = unsafe
    ).


determine_block(unknown, false).
determine_block(safe, false).
determine_block(unsafe, true).


%%	app_unload(+Request) is det.
%

app_unload(Request) :-
    http_parameters(Request,
            [ app(App, [])
            ]),
    app_to_file(App, File),
    % TBD: Make sure none of the unloaded clauses are active
    unload_file(File),
    retractall(loaded(App, _)),
    assert(loaded(App, false)),
    findall(PIA, 
        (   retract(safe(App, PI, _)), 
            assert(safe(App, PI, unknown)),
            retract(blocked(App, PI, _)), 
            assert(blocked(App, PI, false)), 
            term_to_atom(PI, PIA)
        ), 
        List
    ),
    reply_json(json([app=App, unloaded= @true, unblocked=List])).
        

%%	app_block_pred(+Request) is det.
%

app_block_pred(Request) :-
    http_parameters(Request,
            [ app(App, []),
              pi(PIA, [])
            ]),
    block(App, PIA, true).


%%	app_unblock_pred(+Request) is det.
%

app_unblock_pred(Request) :-
    http_parameters(Request,
            [ app(App, []),
              pi(PIA, [])
            ]),
    block(App, PIA, false).
    
    
            
block(App, PIA, Bool) :-
    atom_to_term(PIA, PI, _),
    retractall(blocked(App, PI, _)),
    assert(blocked(App, PI, Bool)),
    reply_json(json([app=App, pred=PIA, blocked= @Bool])).



%%	pengine_permitted(+Goal, -App) is det.
%
%   True if each subgoal in Goal matches a predicate ex§ported by App 
%   and if App is loaded and not blocked. Means that Pengine allows 
%   Goal to be executed. 

pengine_permitted(Var, _App) :- 
    var(Var), !, fail.
pengine_permitted((Goal, Goals), App) :- !,
    pengine_permitted(Goal, App),
    pengine_permitted(Goals, App).
pengine_permitted((Goal ; Goals), App) :- !,
    pengine_permitted(Goal, App),
    pengine_permitted(Goals, App).
pengine_permitted((Goal -> Goals), App) :- !,
    pengine_permitted(Goal, App),
    pengine_permitted(Goals, App).
pengine_permitted(\+(Goal), App) :- !,
    pengine_permitted(Goal, App).
pengine_permitted(once(Goal), App) :- !,
    pengine_permitted(Goal, App).
pengine_permitted(forall(Goal1, Goal2), App) :- !,
    pengine_permitted(Goal1, App),
    pengine_permitted(Goal2, App).
pengine_permitted(findall(_, Goal, _), App) :- !,
    pengine_permitted(Goal, App).
pengine_permitted(aggregate(_, Goal, _), App) :- !,
    pengine_permitted(Goal, App).
pengine_permitted(bag(_, _, Goal, _), App) :- !,
    pengine_permitted(Goal, App).
pengine_permitted(input(_), _App) :- !.
pengine_permitted(input(_, _), _App) :- !.
pengine_permitted(set_prompt(_), _App) :- !.
pengine_permitted(output(_), _App) :- !.
pengine_permitted(fetch(_, _), _App) :- !.
pengine_permitted(Goal, _App) :-
    sandbox:safe_primitive(Goal), !.
pengine_permitted(Goal, _App) :-
    sandbox:safe_primitive(system:Goal), !.
pengine_permitted(Goal, App) :-
    callable(Goal),
    functor(Goal, F, N),
    pred_to_app(F/N, App),
    loaded(App, true),
    blocked(App, F/N, false).
    
    
    
    
    
    
    
    




	
	