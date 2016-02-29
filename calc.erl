-module(calc).
-export([rpn/1]).

rpn(L) when is_list(L) ->
    [Res] = lists:foldl(fun rpn/2, [], string:tokens(L, " ")),
    Res.

rpn("+", [N1, N2|Stack]) -> [N2+N1|Stack];
rpn("-", [N1, N2|Stack]) -> [N2-N1|Stack];
rpn("*", [N1, N2|Stack]) -> [N2*N1|Stack];
rpn("/", [N1, N2|Stack]) -> [N2/N1|Stack];
rpn("^", [N1,N2|S]) -> [math:pow(N2,N1)|S];
rpn("ln", [N|S])    -> [math:log(N)|S];
rpn("log10", [N|S]) -> [math:log10(N)|S];
rpn(N, Stack) -> [read(N)|Stack].


read(N) ->
    case string:to_float(N) of
        {error, no_float} -> list_to_integer(N);
        {F, _} -> F
    end.


