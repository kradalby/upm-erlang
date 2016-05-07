-module(counter_qc).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

initial_state() ->
    [].

new_pre(State) ->
    length(State) < 3.

new_args(_State) ->
    [int()].

new(InitialValue) ->
    counter:new(InitialValue).

new_next(State, Counter, [InitialValue]) ->
    [{Counter, InitialValue}|State].


get_pre(State) ->
    length(State) > 0.

get_pre(State, [Counter]) ->
    lists:any(fun ({Counter1, _}) ->
                      Counter1 == Counter
              end,
              State).

get_args(State) ->
    ?LET(Counter,
         oneof(State),
         [element(1, Counter)]
        ).

get(Counter) ->
    counter:gett(Counter).

get_post(State, [Counter], Return) ->
    {_Pid, Value} = lists:keyfind(Counter, 1, State),
    Value == Return.


set_pre(State) ->
    length(State) > 0.

set_pre(State, [_, Counter]) ->
        lists:any(fun ({Counter1, _}) ->
                      Counter1 == Counter
              end,
              State).

set_args(State) ->
    ?LET(Counter,
         oneof(State),
         [int(), element(1, Counter)]
        ).

set(Value, Counter) ->
    counter:set(Value, Counter).

set_next(State, _OldValue, [NewValue, Counter]) ->
    lists:keyreplace(Counter, 1, State, {Counter, NewValue}).

set_post(State, [_NewValue, Counter], OldValue) ->
    {_Pid, Value} = lists:keyfind(Counter, 1, State),
    Value == OldValue.


inc_pre(State) ->
    length(State) > 0.

inc_pre(State, [Counter]) ->
        lists:any(fun ({Counter1, _}) ->
                      Counter1 == Counter
              end,
              State).

inc_args(State) ->
    ?LET(Counter,
         oneof(State),
         [element(1, Counter)]
        ).

inc(Counter) ->
    counter:inc_atomic(Counter).

inc_next(State, _OldValue, [Counter]) ->
    {_Pid, Value} = lists:keyfind(Counter, 1, State),
    lists:keyreplace(Counter, 1, State, {Counter, Value + 1}).


dec_pre(State) ->
    length(State) > 0.

dec_pre(State, [Counter]) ->
        lists:any(fun ({Counter1, _}) ->
                      Counter1 == Counter
              end,
              State).

dec_args(State) ->
    ?LET(Counter,
         oneof(State),
         [element(1, Counter)]
        ).

dec(Counter) ->
    counter:dec_atomic(Counter).

dec_next(State, _OldValue, [Counter]) ->
    {_Pid, Value} = lists:keyfind(Counter, 1, State),
    lists:keyreplace(Counter, 1, State, {Counter, Value - 1}).


sample() ->
  eqc_gen:sample(eqc_statem:commands(?MODULE)).

eqc() ->
  eqc:quickcheck(?MODULE:prop_s1()).

prop_s1() ->
  ?FORALL(Cmds,commands(?MODULE),
	  begin
	    {H,S,Res} = run_commands(?MODULE,Cmds),
            pretty_commands(?MODULE, Cmds, {H, S, Res},
                            Res == ok)
	  end).

eqcp() ->
  eqc:quickcheck(?MODULE:prop_s2()).

prop_s2() ->
  ?FORALL(Cmds,parallel_commands(?MODULE),
	  begin
	    {H,S,Res} = run_parallel_commands(?MODULE,Cmds),
            pretty_commands(?MODULE, Cmds, {H, S, Res},
                            Res == ok)
	  end).
