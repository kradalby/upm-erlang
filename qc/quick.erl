#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname factorial -mnesia debug verbose

% -mode(compile).
-include_lib("eqc/include/eqc.hrl").


main(_) ->
    eqc:quickcheck(prop_fib()),
    eqc:quickcheck(prop_fib2()),
    eqc:quickcheck(prop_sum()),
    eqc:quickcheck(prop_member()),
    eqc:quickcheck(prop_insert()),
    eqc:quickcheck(prop_mergesort()),
    eqc:quickcheck(prop_quicksort()),
    eqc:quickcheck(prop_quicksort_lc()).

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

prop_quicksort_lc() ->
    ?FORALL({List}, {list(int())}, 
            lists:sort(List) == ex1:quicksort_lc(List)).
