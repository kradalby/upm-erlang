-module(client).

-export([
    permission_to_print/0,
    release_printer/0,
    crashing_client/0,
    sleeping_client/0,
    multi_clients/0
]).

permission_to_print() ->
    gen_server:call(printserver, print, infinity).

release_printer() ->
    gen_server:call(printserver, finish, infinity).


% For testing purposes.
crashing_client() ->
    spawn(fun() ->
        permission_to_print(),
        timer:sleep(3500),
        2/0,
        release_printer()
    end).

sleeping_client() ->
    spawn(fun() ->
        permission_to_print(),
        timer:sleep(5000),
        release_printer(),
        timer:sleep(1000000000)
    end).

multi_clients() ->
    crashing_client(),
    timer:sleep(600),
    sleeping_client(),
    timer:sleep(600),
    crashing_client(),
    crashing_client(),
    crashing_client(),
    timer:sleep(600),
    sleeping_client(),
    timer:sleep(600),
    crashing_client(),
    timer:sleep(600),
    sleeping_client(),
    sleeping_client(),
    sleeping_client(),
    timer:sleep(600),
    crashing_client(),
    timer:sleep(600),
    sleeping_client().
