-module(curling_scoreboard).
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

handle_event({set_teams, Team1, Team2}, State) ->
    curling_scoreboard_hw:set_teams(Team1, Team2),
    {ok, State};

handle_event({add_points, Team, N}, State) ->
    [curling_scoreboard_hw:add_point(Team) || _ <- lists:seq(1, N)],
    {ok, State};

handle_event(next_round, State) ->
    curling_scoreboard_hw:next_round(),
    {ok, State};

handle_event(_, State) ->
    {ok, State}.

handle_info(_, State)  ->
    {ok, State}.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
