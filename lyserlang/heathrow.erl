-module(heathrow).

-compile(export_all).


main() ->
    File = "road.txt",
    {ok, Binary} = file:read_file(File),
    parse_map(Binary).

parse_map(Binary) when is_binary(Binary) ->
    parse_map(binary_to_list(Binary));
parse_map(String) when is_list(String) ->
    Values = [list_to_integer(Element) || Element <- string:tokens(String, "\r\t\n ")],
    group_values(Values, []).

group_values([], Acc) ->
    lists:reverse(Acc);
group_values([A, B, X|Rest], Acc) ->
    group_values(Rest, [{A, B, X}|Acc]).
