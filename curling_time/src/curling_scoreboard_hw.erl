-module(curling_scoreboard_hw).
-export([
    add_point/1,
    next_round/0,
    set_teams/2,
    reset_board/0
]).

add_point(Team) ->
    io:format("Team: ~p, got 1 more point~n", [Team]).

next_round() ->
    io:format("New round!~n").

set_teams(Team1, Team2) ->
    io:format("~p playing against ~p~n", [Team1, Team2]).

reset_board() ->
    io:format("resetting board...~n").
