-module(trade_fsm).
-behaviour(gen_fsm).

% Public API
-export([
    start/1,
    start_link/1,
    trade/2,
    accept_trade/1,
    make_offer/1,
    retract_offer/2,
    ready/1,
    cancel/1
]).

% behaviour callbacks
-export([
    init/1,
    handle_event/3,
    handle_sync_event/4,
    handle_info/3,
    terminate/3,
    code_change/4,

    % Custom states
    idle/2,
    idle/3,
    idle_wait/2,
    idle_wait/3,
    negotiate/2,
    wait/2,
    ready/2,
    ready/3
]).

-record(state, {
    name="",
    other,
    ownitems=[],
    otheritems=[],
    monitor,
    from
}).

start(Name) ->
    gen_fsm:start(?MODULE, [Name], []).

start_link(Name) ->
    gen_fsm:start_link(?MODULE, [Name], []).

trade(MyPid, OtherPid) ->
    gen_fsm:sync_send_event(OwnPid, {negotiate, OtherPid}, 30000).

accept_trade(OwnPid) ->
    gen_fsm:sync_send_event(OwnPid, accept_negotiate).

make_offer(OwnPid, Item) ->
    gen_fsm:send_event(OwnPid, {make_offer, Item}).

retract_offer(OwnPid, Item) ->
    gen_fsm:send_event(OwnPid, {retract_offer, Item}).

ready(OwnPid) ->
    gen_fsm:sync_send_event(OwnPid, ready, infinity).

cancel(OwnPid) ->
    gen_fsm:sync_send_all_state_event(OwnPid, cancel).

% FSM to FSM
ask_negotiate(OtherPid, OwnPid) ->
    gen_fsm:send_event(OtherPid, {ask_negotiate, OwnPid}).

accept_negotiate(OtherPid, OwnPid) ->
    gen_fsm:send_event(OtherPid, {accept_negotiate, OwnPid}).

do_offer(OtherPid, Item) ->
    gen_fsm:send_event(OtherPid, {do_offer, Item}).

undo_offer(OtherPid, Item) ->
    gen_fsm:send_event(OtherPid, {undo_offer, Item}).

are_you_ready(OtherPid) ->
    gen_fsm:send_event(OtherPid, are_you_ready).

not_yet(OtherPid) ->
    gen_fsm:send_event(OtherPid, not_yet).

am_ready(OtherPid) ->
    gen_fsm:send_event(OtherPid, 'ready!').


ack_trans(OtherPid) ->
    gen_fsm:send_event(OtherPid, ack).

ask_commit(OtherPid) ->
    gen_fsm:sync_send_event(OtherPid, ask_commit).

do_commit(OtherPid) ->
    gen_fsm:sync_send_event(OtherPid, do_commit).

notify_cancel(OtherPid) ->
    gen_fsm:sync_send_all_state_event(OtherPid, cancel).


% FSM callback handlers
init(Name) ->
    {ok, idle, #state{name=Name}}.

notice(#state{name=N}, Str, Args) ->
    io:format("~s: "++Str++"~n", [N|Args]).

unexpected(Message, State) ->
    io:format("~p received unkown event ~p while in state ~p~n",
        [self(), Message, State]).
