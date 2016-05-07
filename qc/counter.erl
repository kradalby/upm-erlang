-module(counter).
-behaviour(gen_server).

-export([
         handle_call/3,
         handle_info/2,
         handle_cast/2,
         code_change/3,
         init/1,
         terminate/2
]).
-export([
         new/1,
         gett/1,
         set/2,
         inc/1,
         dec/1,
         inc_atomic/1,
         dec_atomic/1
]).

init(Args) ->
    {ok, Args}.

handle_call({get}, _From, State) ->
    {reply, State, State};

handle_call({set, Value}, _From, State) ->
    {reply, State, Value};

handle_call({inc}, _From, State) ->
    {reply, State, State + 1};

handle_call({dec}, _From, State) ->
    {reply, State, State - 1};

handle_call(Message, From, State) ->
    io:format("Got ~p from ~p~n", [Message, From]),
    {reply, ok, State}.

handle_info(Message, State) ->
    io:format("Got unexpected message ~p~n", [Message]),
    {noreply, State}.
    
handle_cast(Message, State) ->
    io:format("Got unexpected message ~p~n", [Message]),
    {noreply, State}.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

new(InitValue) ->
    {ok, Pid} = gen_server:start(?MODULE, InitValue,[]),
    Pid.

gett(Counter) ->
    gen_server:call(Counter, {get}).

set(Value, Counter) ->
    gen_server:call(Counter, {set, Value}).

inc(Counter) ->
    set(gett(Counter) + 1, Counter).

dec(Counter) ->
    set(gett(Counter) - 1, Counter).

inc_atomic(Counter) ->
    gen_server:call(Counter, {inc}).

dec_atomic(Counter) ->
    gen_server:call(Counter, {dec}).
