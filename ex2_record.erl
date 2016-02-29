-module(ex2_record).

-compile(export_all).

-record(node, {value, left, right}).

empty() ->
    #node{left=void, right=void}.

insert(Value, #node{left=void, right=void}) ->
    #node{value=Value, left=empty(), right=empty()};
insert(Value, #node{value=Node_value, left=Left, right=Right}) when Value < Node_value ->
    #node{value=Node_value, left=insert(Value, Left), right=Right};
insert(Value, #node{value=Node_value, left=Left, right=Right}) when Value > Node_value ->
    #node{value=Node_value, left=Left, right=insert(Value, Right)};
insert(Value, #node{value=_, left=Left, right=Right}) ->
    #node{value=Value, left=Left, right=Right}.


treemap2(_, void) ->
    void;
treemap2(F, #node{value=Value, left=Left, right=Right}) ->
    #node{value=F(Value), left=treemap2(F, Left), right=treemap2(F, Right)}.

treemap(F, #node{value=Value, left=void, right=void}) ->
    #node{value=F(Value), left=void, right=void};
treemap(F, #node{value=Value, left=void, right=Right}) ->
    #node{value=F(Value), left=void, right=treemap(F, Right)};
treemap(F, #node{value=Value, left=Left, right=void}) ->
    #node{value=F(Value), left=treemap(F, Left), right=void};
treemap(F, #node{value=Value, left=Left, right=Right}) ->
    #node{value=F(Value), left=treemap(F, Left), right=treemap(F, Right)}.

treefold(F, Acc, #node{value=Value, left=void, right=void}) ->
    F(Value, Acc);
treefold(F, Acc, #node{value=Value, left=Left, right=void}) ->
    treefold(F, F(Value, Acc), Left);
treefold(F, Acc, #node{value=Value, left=void, right=Right}) ->
    treefold(F, F(Value, Acc), Right);
treefold(F, Acc, #node{value=Value, left=Left, right=Right}) ->
    treefold(F, F(Value, treefold(F, Acc, Right)), Left).

find(Pred, #node{value=Value, left=void, right=void}) ->
    case Pred(Value) of
        true -> {ok, Value};
        false -> false
    end;
find(Pred, #node{value=Value, left=Left, right=void}) ->
    case Pred(Value) of
        true -> {ok, Value};
        false -> find(Pred, Left)
    end;
find(Pred, #node{value=Value, left=void, right=Right}) ->
    case Pred(Value) of
        true -> {ok, Value};
        false -> find(Pred, Right)
    end;
find(Pred, #node{value=Value, left=Left, right=Right}) ->
    case Pred(Value) of
        true -> {ok, Value};
        false -> find(Pred, Left), find(Pred, Right)
    end.

find2(_, void) ->
    false;
find2(Pred, #node{value=Value, left=Left, right=Right}) ->
    case Pred(Value) of
        true -> {ok, Value};
        false -> case find2(Pred, Left) of
                     false -> find2(Pred, Right);
                     {ok, T} -> {ok, T}
                end
    end.

