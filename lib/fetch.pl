:- module(fetch,
        [   fetch/2
        ]).
        
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).


fetch(URL, Json) :-    
    http_open(URL, Stream, []),
    json_read(Stream, Json),
    close(Stream).
