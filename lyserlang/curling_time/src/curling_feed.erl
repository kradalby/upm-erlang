-module(curling_feed).
-behaviour(gen_event).

-export([
    init/1,
    terminate/2,
    handle_call/2,
    handle_event/2,
    handle_info/2,
    code_change/3
]).

init(State) ->
    {ok, State}.

terminate(_Reason, _State)  ->
    {ok}.

handle_call(_, State) ->
    {ok, ok, State}.

handle_event(Event, [Pid]) ->
    Pid ! {curling_feed, Event},
    {ok, [Pid]}.

handle_info(_, State)  ->
    {ok, State}.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
