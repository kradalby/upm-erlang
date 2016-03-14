-module(myrpc).
-export([
         start/0,
         start_watcher/0,
         terminate/0,
         crash/0,
         appply/2,
         appply/3,
         apply_after/3
        ]).

uuid() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:rand_bytes(16),
    Str = io_lib:format("~8.16.0b-~4.16.0b-4~3.16.0b-~4.16.0b-~12.16.0b",
                        [A, B, C band 16#0fff, D band 16#3fff bor 16#8000, E]),
    list_to_binary(Str).

start() ->
    Pid = spawn(fun loop/0),
    register(myrpc, Pid).

start_watcher() ->
    Pid = spawn(fun watcher/0),
    register(watcher, Pid),
    watcher ! {monitor, whereis(myrpc)}.


terminate() ->
    case whereis(myrpc) of
        undefined -> dead;
        Pid when is_pid(Pid) ->
            exit(Pid, kill),
            dead
    end.

crash() ->
    myrpc ! {crash}.

watcher() ->
    receive
        {monitor, Pid} -> monitor(process, Pid);
        {'DOWN', MonitorRef, Type, Object, Info} ->
            start(),
            monitor(process, myrpc);
        X -> io:format("~p~n", [X])
    end,
    watcher().

loop() ->
    io:format("~p ~p ~n", [whereis(myrpc), self()]),
    receive
        {apply, Fun, Args, Pid, ID} ->
            spawn(fun() ->
                          try
                              Pid ! {result, ID, apply(Fun, Args)}
                          catch
                              Exception:Reason -> Pid ! {error, ID, {Exception, Reason}}
                          end
                  end);
        {crash} -> 2/0
    end,
    loop().

appply(Fun, Args) ->
    appply(Fun, Args, infinity).

appply(Fun, Args, Timeout) ->
    io:format("~p ~p ~n", [whereis(myrpc), self()]),
    ID = uuid(),
    case whereis(myrpc) of
        undefined -> server_crashed;
        MyrpcPid when is_pid(MyrpcPid) ->
            myrpc ! {apply, Fun, Args, self(), ID},
            receive
                {result, ID, Result} -> Result;
                {error, ID, Error} -> Error
            after
                Timeout -> timeout
            end
    end.


apply_after(Delay, Fun, Args) ->
    receive
    after
        Delay -> appply(Fun, Args)
    end.


% internal_apply(Fun, Args) ->
