-module(printserver).
-behaviour(gen_server).


-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).

-export([
    start/1,
    stop/0,
    queue/0
]).

init(State) ->
    {ok, State}.

terminate(_Reason, State) ->
    io:format("Exits"),
    [gen_server:reply(From, shutting_down) || {_MonitorRef, From} <- State],
    ok.

handle_call(print, From, []) ->
    {Pid, _} = From,
    MonitorRef = monitor(process, Pid),
    NewState = [{MonitorRef, From}],
    io:format("Print,   "),
    print_current_queue(NewState),
    {reply, ok, NewState};

handle_call(print, From, State) ->
    {Pid, _} = From,
    MonitorRef = monitor(process, Pid),
    NewState = State ++ [{MonitorRef, From}],
    io:format("Print,   "),
    print_current_queue(NewState),
    {noreply, NewState};

handle_call(finish, From, []) ->
    io:format("Recieved a finish from ~p, but the queue is empty...~n", [From]),
    {reply, ok, []};

handle_call(finish, _From, [_H|T]) ->
    io:format("Finish,   "),
    print_current_queue(T),
    case T of
        [] -> {reply, ok, []};
        [{_MonitorRef, NewFrom}|_Tail] -> gen_server:reply(NewFrom, ok), {reply, ok, T}
    end.

handle_cast(queue, State) ->
    print_current_queue(State),
    {noreply, State};

handle_cast(_Pattern, State) ->
    {noreply, State}.

handle_info({'DOWN', MonitorRef, _Type, _Object, _Info}, State) ->
    io:format("Received DOWN message from: ~p~n", [MonitorRef]),
    case lists:keyfind(MonitorRef, 1, State) of
        false -> io:format("Crashed process not in queue"), {noreply, State};
        {MonitorRef, From} ->
            NewState = lists:delete({MonitorRef, From}, State),
            [{_NewMonitorRef, NewFrom}|_T] = NewState,
            gen_server:reply(NewFrom, ok),
            io:format("Crash,   "),
            print_current_queue(NewState),
            {noreply, NewState}
    end;

handle_info(Info, State) ->
    io:format("Unexpected message: ~p~n", [Info]),
    {noreply, State}.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

print_current_queue(Queue) ->
    io:format("Current print queue: ~p~n", [Queue]).

start([]) ->
    {ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    Pid.

stop() ->
    gen_server:stop(printserver).

queue() ->
    gen_server:cast(printserver, queue).
