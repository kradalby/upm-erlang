-module(event).
-export([]).
-compile(export_all).

-record(state, {
    server,
    name="",
    time_left=[]
}).

start(Name, Delay) ->
    spawn(?MODULE, init, [self(), Name, Delay]).

start_link(Name, Delay) ->
    spawn_link(?MODULE, init, [self(), Name, Delay]).

init(Server, Name, Delay) ->
    loop(#state{
        server=Server,
        name=Name,
        time_left=time_to_go(Delay)
    }).

cancel(Pid) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, cancel},
    receive
        {Ref, ok} ->
            erlang:demonitor(Ref, [flush]),
            ok;
        {'DOWN', Ref, process, Pid, _Reason} ->
            ok
    after
        5000 -> timeout
    end.

loop(S = #state{server = Server, time_left=[Head|Tail]}) ->
    receive
        {Server, Ref, cancel} ->
            Server ! {Ref, ok}
    after
        Head * 1000 ->
            if Tail =:= [] ->
                 Server ! {done, S#state.name};
               Tail =/= [] ->
                 loop(S#state{time_left=Tail})
            end
    end.

normalize(N) ->
    Limit = 49 * 24 * 60 * 60,
    [N rem Limit | lists:duplicate(N div Limit, Limit)].

time_to_go(TimeOut={{_, _, _}, {_, _, _}}) ->
    Now = calendar:local_time(),
    ToGo = calendar:datetime_to_gregorian_seconds(TimeOut) -
           calendar:datetime_to_gregorian_seconds(Now),
    Seconds = if ToGo > 0 -> ToGo;
                 ToGo =< 0 -> 0
             end,
    normalize(Seconds).
