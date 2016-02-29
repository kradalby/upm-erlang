-module(ex1).
-export([fib/1,
         fib_tail/1,
         fib_case/1,
         fib_if/1,
         sum/1,
         member/2,
         insert/2,
         mergesort/1,
         quicksort/1,
         quicksort_lc/1,
         fib_with_index/1,
         keyfind/2,
         merge/2]
       ).
-compile(export_all).

% Exercise 0
fib(0) ->
    0;
fib(1) ->
    1;
fib(N) when is_integer(N), N > 0 ->
    fib(N-1) + fib(N-2).

fib_tail(N) -> fib_tail(N, 0, 1).

fib_tail(0, N1, _) -> N1;
fib_tail(N, N1, N2) ->
    fib_tail(N-1, N2, N1 + N2).


fib_case(N) ->
    case N of
        0 -> 0;
        1 -> 1;
        _ when is_integer(N), N > 0 -> fib_case(N-1) + fib_case(N-2)
    end.


fib_if(N) ->
    if N =:= 0 -> 0;
       N =:= 1 -> 1;
       is_integer(N), N > 0 -> fib_if(N-1) + fib_if(N-2)
    end.

% Exercise 1
sum([]) ->
    0;
sum([H|T]) ->
    if is_integer(H) -> H + sum(T);
       true -> sum(T)
    end.

% Exercise 2
member(_, []) ->
    false;
member(E, [H|T]) ->
    case H of
        E -> true;
        _ -> member(E, T)
    end.


% Exercise 2.5
% 3 [1,2,4,5,6]
% ([1] + ([2] + ([3] + [4,5,6])))

insert(E, []) ->
    [E];
insert(E, [H|T]) when E =< H ->
    [E] ++ [H] ++ T;
insert(E, [H|T]) when E > H ->
    [H] ++ insert(E, T).

% Exercise 3

%[5,4,3,2,5,6]
%[5,4,3] [2,5,6]
%[5,4] [3] [2,5] [6]
%[5] [4] [3] [2] [5] [6]

% Merge sort, using merge from Exercise 6
mergesort(List) when length(List) =< 1 -> List;
mergesort(List) ->
    {Left, Right} = lists:split(length(List) div 2, List),
    merge(mergesort(Left), mergesort(Right)).

quicksort([]) -> [];
quicksort([P|Rest]) ->
    {Left, Right} = partition(P, Rest, [], []),
    quicksort(Left) ++ [P] ++ quicksort(Right).

partition(_, [], Left, Right) -> {Left, Right};
partition(P, [H|T], Left, Right) ->
    if H =< P -> partition(P, T, [H|Left], Right);
       H >  P -> partition(P, T, Left, [H|Right])
    end.

quicksort_lc([]) -> [];
quicksort_lc([P|Rest]) ->
    quicksort_lc([Left || Left <- Rest, Left =< P])
    ++ [P] ++
    quicksort_lc([Right || Right <- Rest, Right > P]).

% Exercise 4

fib_with_index(0) ->
    {0, 0};
fib_with_index(1) ->
    {1, 1};
fib_with_index(N) when is_integer(N), N > 0 ->
    {N, fib(N)}.

% Exercise 5

keyfind(_, []) -> false;
keyfind(K, [H|T]) ->
    {Key, _} = H,
    case Key of
        K -> H;
        _ -> keyfind(K, T)
    end.

% Exercise 6
merge([], L) -> L;
merge(L, []) -> L;
merge([HX|TX], [HY|TY]) when HX < HY ->
    [HX] ++ merge(TX, [HY] ++ TY);
merge([HX|TX], [HY|TY]) ->
    [HY] ++ merge([HX] ++ TX, TY).


map([], _F) -> [];
map([H|T], F) ->
    [F(H)|map(T, F)].

increment(N) -> N + 1.

filter(L, F) ->
    foldl(fun(E, L) ->
            case F(E) of
                true  -> [E|L];
                false -> L
            end
        end, [], L).

foldl(_, Acc, []) -> Acc;
foldl(F, Acc, [H|T]) ->
    foldl(F, F(H, Acc), T).
