-module(musicians).
-behaviour(gen_server).

-export([
    start_link/2,
    stop/1
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-define(DELAY, 750).

-record(state, {
    name="",
    role,
    skill="good"
}).

start_link(Role, Skill) ->
    gen_server:start_link({local, Role}, ?MODULE, [Role, Skill], []).

stop(Role) ->
    gen_server:call(Role, stop).

init([Role, Skill]) ->
    process_flag(trap_exit, true),
    random:seed(now()),
    TimeToPlay = random:uniform(3000),
    Name = pick_name(),
    StrRole = atom_to_list(Role),
    io:format("Musician ~s, playing the ~s entered the room~n",
        [Name, StrRole]),
    {ok, #state{name=Name, role=StrRole, skill=Skill}, TimeToPlay}.

handle_call(stop, _From, State=#state{}) ->
    {stop, normal, ok, State};

handle_call(_Message, _From, State=#state{}) ->
    {noreply, State, ?DELAY}.

handle_cast(_Event, State) ->
    {noreply, State, ?DELAY}.

handle_info(timeout, State = #state{name = Name, skill=good}) ->
    io:format("~s produced sound! ~n", [Name]),
    {noreply, State, ?DELAY};

handle_info(timeout, State = #state{name = Name, skill=bad}) ->
    case random:uniform(5) of
        1 ->
            io:format("~s played a false note. Uh oh, dick~n", [Name]),
            {stop, bad_note, State};
        _ ->
            io:format("~s produced sound! ~n", [Name]),
            {noreply, State, ?DELAY}
    end;

handle_info(_Event, State) ->
    {noreplay, State, ?DELAY}.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

terminate(normal, State) ->
    io:format("~s left the room (~s)~n", [State#state.name, State#state.role]);

terminate(bad_note, State) ->
    io:format("~s sucks! kicked that member out of the band! (~s)~n",
        [State#state.name, State#state.role]);

terminate(shutdown, State) ->
    io:format("The manager is mad and fired the whole band!
        ~s just got back to playing in the subway~n",
        [State#state.name]);

terminate(_Reason, State) ->
    io:format("~s has been kicked out (~s)~n",
        [State#state.name, State#state.role]).

pick_name() ->
    lists:nth(random:uniform(10), firstname())
    ++ " " ++
    lists:nth(random:uniform(10), lastnames()).

firstname() ->
    ["Valerie", "Arnold", "Carlos", "Dorothy", "Keesha",
    "Phoebe", "Ralphie", "Tim", "Wanda", "Janet"].

lastnames() ->
    ["Frizzle", "Perlstein", "Ramon", "Ann", "Franklin",
    "Terese", "Tennelli", "Jamal", "Li", "Perlstein"].
