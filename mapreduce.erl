-module(mapreduce).
-export([mapreduce/3]).

mapreduce(Data, Map, Reduce) ->
    Pid = self(),
    spawn(fun() -> manager(Pid, Data, Map, Reduce)) end).
    receive
        {result, Result} -> Result
    end.

manager(Boss, Data, Map, Reduce) ->
    Pid = self(),
    io:format("~p~n", [Data]),

    MapPids = spawner(Pid, Data, Fun, []),
    io:format("~p~n", [MapPids]),




spawner(_Manager, [], _Fun, Pids) -> Pids;
spawner(Manager, [H|T], Fun, Pids) ->
    {Key, Value} = H,
    Pid = spawn(fun() -> Manager ! Fun(Key, Value) end),
    spawner(T, [Pid|Pids], Fun).


collect(0, List) -> List;
collect(W, List) ->
    io:format("~p~n", [List]),
    receive
        {data, Key, Value} ->
            collect(W, [{Key, Value}|List]);
        {done} ->
            collect(W-1, List)
    end.
