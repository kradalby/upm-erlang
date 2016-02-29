-module(ex2).

-compile(export_all).

all(_, []) -> true;
all(F, [H|T]) ->
    case F(H) of
        true -> all(F, T);
        false -> false
    end.

foldl(_, Start, []) -> Start;
foldl(F, Start, [H|T]) ->
    foldl(F, F(H, Start), T).

foldr(_, Start, []) -> Start;
foldr(F, Start, [H|T]) ->
    F(H, foldr(F, Start, T)).

% ex2:filter(fun(A) -> A >= 7 end, [5,6,7,8]).
filter(_, []) -> [];
filter(Pred, L) ->
    F = fun(X, Acc) ->
            case Pred(X) of
                true -> [X|Acc];
                false -> Acc
            end
        end,
    foldl(F, [], L).

% ex2:map(fun(A) -> A*2 end, [5,6,7,8]).
map(F, L) ->
    foldl(fun (X, Acc) -> [F(X)|Acc] end, [], L).

natmap(_, []) -> [];
natmap(F, [H|T]) ->
    [F(H) | natmap(F, T)].


treemap2(_, void) ->
    void;
treemap2(F, {node, Key, Left, Right}) ->
    {node, F(Key), treemap2(F ,Left), treemap2(F, Right)}.

treemap(F, {node, Key, void, void}) ->
    {node, F(Key), void, void};
treemap(F, {node, Key, void, Right}) ->
    {node, F(Key), void, treemap(F, Right)};
treemap(F, {node, Key, Left, void}) ->
    {node, F(Key), treemap(F, Left), void};
treemap(F, {node, Key, Left, Right}) ->
    {node, F(Key), treemap(F, Left), treemap(F, Right)}.

treefold(F, Acc, {node, Key, void, void}) ->
    F(Key, Acc);
treefold(F, Acc, {node, Key, Left, void}) ->
    treefold(F, F(Key, Acc), Left);
treefold(F, Acc, {node, Key, void, Right}) ->
    treefold(F, F(Key, Acc), Right);
treefold(F, Acc, {node, Key, Left, Right}) ->
    treefold(F, F(Key, treefold(F, Acc, Right)), Left).

find(Pred, {node, Key, void, void}) ->
    case Pred(Key) of
        true -> {ok, Key};
        false -> false
    end;
find(Pred, {node, Key, Left, void}) ->
    case Pred(Key) of
        true -> {ok, Key};
        false -> find(Pred, Left)
    end;
find(Pred, {node, Key, void, Right}) ->
    case Pred(Key) of
        true -> {ok, Key};
        false -> find(Pred, Right)
    end;
find(Pred, {node, Key, Left, Right}) ->
    case Pred(Key) of
        true -> {ok, Key};
        false -> find(Pred, Left), find(Pred, Right)
    end.



