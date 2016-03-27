-module(evserv).
-compile(export_all).

-record(state, {events, clients}).

-record(event, {
    name="",
    description="",
    pid,
    timeout={{1970, 1, 1}, {0, 0, 0}}
}).

start() ->
    register(?MODULE, Pid=spawn(?MODULE, init, [])),
    Pid.

start_link() ->
    register(?MODULE, Pid=spawn_link(?MODULE, init, [])),
    Pid.

init() ->
    loop(#state{
        events = orddict:new(),
        clients = orddict:new()
    }).

terminate() ->
    ?MODULE ! {shutdown}.

loop(S = #state{}) ->
    receive
        {Pid, MessageRef, {subscribe, Client}} ->
            Ref = erlang:monitor(process, Pid),
            NewClients = orddict:store(Ref, Client, S#state.clients),
            Pid ! {MessageRef, ok},
            loop(S#state{clients=NewClients});

        {Pid, MessageRef, {add, Name, Description, TimeOut}} ->
            case valid_datetime(TimeOut) of
                true ->
                    EventPid = event:start_link(Name, TimeOut),
                    NewEvents = orddict:store(Name, #event{
                        name = Name,
                        description = Description,
                        pid = EventPid,
                        timeout = TimeOut
                    }, S#state.events),
                    Pid ! {MessageRef, ok},
                    loop(S#state{events = NewEvents});
                error ->
                    Pid ! {MessageRef, {error, bad_timeout}},
                    loop(S)
            end;
        {Pid, MessageRef, {cancel, Name}} ->
            Events = case orddict:find(Name, S#state.events) of
                {ok, E} ->
                    event:cancel(E#event.pid),
                    orddict:erase(Name, S#state.events);
                false ->
                    S#state.events
                end,
                Pid ! {MessageRef, ok},
                loop(S#state{events=Events});
        {done, Name} ->
            case orddict:find(Name, S#state.events) of
                {ok, E} ->
                    send_to_all_clients({done, E#event.name, E#event.description},
                        S#state.clients),
                    NewEvents = orddict:erase(Name, S#state.events),
                    loop(S#state{events=NewEvents});
                error ->
                    loop(S)
            end;
        {shutdown} ->
            exit(shutdown);
        {code_change} ->
            ?MODULE:loop(S);
        {'DOWN', Ref, process, _Pid, _Reason} ->
            loop(S#state{clients=orddict:erase(Ref, S#state.clients)});
        Unknown ->
            io:format("Unknown message: ~p ~n", [Unknown]),
            loop(S)
    end.

valid_datetime({Date, Time}) ->
    try
        calendar:valid_date(Date) andalso valid_time(Time)
    catch
        error:function_clause ->
            false
    end;
valid_datetime(_) ->
    false.

valid_time({H, M, S}) ->
    valid_time(H, M, S).
valid_time(H, M, S) when
    H >= 0,
    H < 24,
    M >= 0,
    M < 60,
    S >= 0,
    S < 60 ->
        true;
valid_time(_, _, _) ->
    false.

send_to_all_clients(Payload, Clients) ->
    orddict:map(fun(_Ref, Pid) -> Pid ! Payload end, Clients).




subscribe(Pid) ->
    Ref = erlang:monitor(process, ?MODULE),
    ?MODULE ! {self(), Ref, {subscribe, Pid}},
    receive
        {Ref, ok} -> ok;
        {'DOWN', Ref, process, _Pid, Reason} ->
            {error, Reason}
    after
        5000 ->
        {error, timout}
    end.

add_event(Name, Description, TimeOut) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {add, Name, Description, TimeOut}},
    receive
        {Ref, ok} -> ok
    after
        5000 ->
            {error, timeout}
    end.

cancel(Name) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {cancel, Name}},
    receive
        {Ref, ok} -> ok
    after
        5000 ->
            {error, timeout}
    end.
