Eval: 


<<<<<<< Updated upstream

main(_) ->
    io:format("~nRunning: fib~n"),
    eqc:quickcheck(prop_fib()),
    io:format("~nRunning: fib2~n"),
    eqc:quickcheck(prop_fib2()),
    io:format("~nRunning: sum~n"),
    eqc:quickcheck(prop_sum()),
    io:format("~nRunning: member~n"),
    eqc:quickcheck(prop_member()),
    io:format("~nRunning: insert~n"),
    eqc:quickcheck(prop_insert()),
    io:format("~nRunning: mergesort~n"),
    eqc:quickcheck(prop_mergesort()),
    io:format("~nRunning: quicksort~n"),
    eqc:quickcheck(prop_quicksort()),
    io:format("~nRunning: quicksort_idempotent~n"),
    eqc:quickcheck(prop_quicksort_idempotent()),
    io:format("~nRunning: quicksort_lc~n"),
    eqc:quickcheck(prop_quicksort_lc()),
    io:format("~nRunning: quicksort_min~n"),
    eqc:quickcheck(prop_quicksort_min()),
    io:format("~nRunning: quicksort_max~n"),
    eqc:quickcheck(prop_quicksort_max()),
    io:format("~nRunning: map~n"),
    eqc:quickcheck(prop_map()),

    eqc:sample(tree()).

prop_fib() ->
    ?FORALL({N}, {int()},
            try ex1:fib(N) == ex1:fib_tail(N)
            catch
                error:_ ->
                    N < 0
            end).

prop_fib2() ->
    ?FORALL({N}, {int()},
            try ex1:fib_if(N) == ex1:fib_case(N)
            catch
                error:_ ->
                    N < 0
            end).

prop_sum() ->
    ?FORALL({List}, {list(int())},
            lists:sum(List) == ex1:sum(List)).

prop_member() ->
    ?FORALL({Element, List}, {int(), list(int())},
            lists:member(Element, List) == ex1:member(Element, List)).

prop_insert() ->
    ?FORALL({Element, List}, {int(), list(int())},
            lists:sort(List ++ [Element]) == ex1:insert(Element, lists:sort(List))).

prop_mergesort() ->
    ?FORALL({List}, {list(int())},
            lists:sort(List) == ex1:mergesort(List)).

prop_quicksort() ->
    ?FORALL({List}, {list(int())},
            lists:sort(List) == ex1:quicksort(List)).

prop_quicksort_idempotent() ->
    ?FORALL({List}, {list(int())},
            ex1:quicksort(ex1:quicksort(List)) == ex1:quicksort(List)).

prop_quicksort_lc() ->
    ?FORALL({List}, {list(int())},
            lists:sort(List) == ex1:quicksort_lc(List)).

prop_quicksort_min() ->
    ?FORALL({List}, {list(int())},
            try
                begin
                    Min = lists:min(List),
                    [H|_] = ex1:quicksort(List),
                    H == Min
                end
            catch
                error:_ ->
                    length(List) == 0
            end).

prop_quicksort_max() ->
    ?FORALL({List}, {list(int())},
            try
                begin
                    Max = lists:max(List),
                    [H|_] = lists:reverse(ex1:quicksort(List)),
                    H == Max
                end
            catch
                error:_ ->
                    length(List) == 0
            end).

prop_map() ->
    ?FORALL({List, Int}, {list(int()), int()},
           begin
                Fun = fun(X) -> X*Int end,
                NewList = ex2:map(Fun, List),
                SumList = lists:sum(List) * Int,
                SumNewList = lists:sum(NewList),
                SumList == SumNewList
            end).

tree() ->
    frequency([
           {100000, {node, int(), void, void}},
           {1, {node, int(), tree(), tree()}},
           {2, {node, int(), void, tree()}},
           {2, {node, int(), tree(), void}}
          ]).

prop_tree_find2() ->
    ?FORALL({Tree}, {},
           begin
               io:format("derp")
           end).
=======
>>>>>>> Stashed changes
