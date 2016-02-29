-module(useless).

-export([
         add/2,
         greet_and_add_two/1,
         len/1,
         duplicate/2,
         reverse/1,
         sublist/2,
         zip/2
        ]).
-compile(export_all).


add(A,B) ->
    A + B.

hello() ->
    io:format("derp~n").


greet_and_add_two(X) ->
    hello(),
    add(X,2).


len(L) -> len_tail(L, 0).

len_tail([], Acc) -> Acc;
len_tail([_|T], Acc) ->
    len_tail(T, Acc + 1).


duplicate(N, Term) -> duplicate_tail(N, Term, []).

duplicate_tail(0, _, Acc) -> Acc;
duplicate_tail(N, Term, Acc) when is_integer(N) ->
    duplicate_tail(N-1, Term, [Term|Acc]).


reverse(L) -> reverse_tail(L, []).

reverse_tail([], List) -> List;
reverse_tail([H|T], List) ->
    reverse_tail(T, [H|List]).


sublist(L, N) -> reverse(sublist_tail(L, N, [])).

sublist_tail([], _, Acc) -> Acc;
sublist_tail(_, 0, Acc) -> Acc;
sublist_tail([H|T], N, Acc) when is_integer(N) ->
    sublist_tail(T, N-1, [H|Acc]).


zip(List1, List2) -> zip_tail(List1, List2, []).

zip_tail([], [], Acc) -> Acc;
zip_tail([H1|T1], [H2|T2], Acc) ->
    zip_tail(T1, T2, [{H1, H2} | Acc]).


map(_, []) -> [];
map(F, [H|T]) ->
    [F(H)|map(F, T)].


filter(F, L) -> filter(F, L, []).

filter(_, [], Acc) -> Acc;
filter(F, [H|T], Acc) ->
    case F(H) of
        true -> filter(F, T, [H|Acc]);
        false -> filter(F, T, Acc)
    end.

fold(_, Start, []) -> Start;
fold(F, Start, [H|T]) ->
    fold(F, F(H, Start), T).

reverse_fold(L) ->
    fold(fun(E, Acc) -> [E|Acc] end, [], L).


filter_fold(Pred, L) ->
    F = fun(X, Acc) ->
        case Pred(X) of
            true -> [X|Acc];
            false -> Acc
        end
    end,
    fold(F, [], L).

