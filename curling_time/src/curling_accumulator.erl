-module(curling_accumulator).
-bevaviour(gen_event).


-export([
    init/1,
    terminate/2,
    handle_call/2,
    handle_event/2,
    handle_info/2,
    code_change/3
]).

-record(state, {
    teams = orddict:new(),
    round = 0
}).

init([]) ->
    {ok, #state{}}.

terminate(_Reason, _State)  ->
    {ok}.

handle_call(game_info, State=#state{teams=Teams, round=Round}) ->
    {ok, {orddict:to_list(Teams), {round, Round}}, State};

handle_call(_, State) ->
    {ok, ok, State}.

handle_event({set_teams, Team1, Team2}, State=#state{teams=Teams}) ->
    NewTeams = orddict:store(Team1, 0, orddict:store(Team2, 0, Teams)),
    {ok, State#state{teams = NewTeams}};

handle_event({add_points, Team, N}, State=#state{teams=Teams}) ->
    NewTeams = orddict:update_counter(Team, N, Teams),
    {ok, State#state{teams = NewTeams}};

handle_event(next_round, State=#state{}) ->
    {ok, State#state{round = State#state.round + 1}};

handle_event(_, State) ->
    {ok, State}.

handle_info(_, State)  ->
    {ok, State}.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
