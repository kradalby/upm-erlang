-module(sup).
-export([
    start/2,
    start_link/2,
    init/1,
    loop/1
]).

start(Module, Arguments) ->
    spawn(?MODULE, init, [{Module, Arguments}]).

start_link(Module, Arguments) ->
    spawn_link(?MODULE, init, [{Module, Arguments}]).


init({Module, Arguments}) ->
    process_flag(trap_exit, true),
    loop({Module, start_link, Arguments}).

loop({Module, Func, Args}) ->
    Pid = apply(Module, Func, Args),
    receive
        {'EXIT', _From, shutdown} ->
            exit(shutdown);
        {'EXIT', Pid, Reason} ->
            io:format("Process ~p exited for reason ~p~n", [Pid, Reason]),
            loop({Module, Func, Args})
    end.
