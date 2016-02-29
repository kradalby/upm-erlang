-module(ex3).

-compile(export_all).


ping() ->
    receive
        {req, Msg, Pid} when is_pid(Pid) -> Pid ! {res, Msg, self()};
        M -> io:format("Got an invalid message: ~p~n", [M])
    end,
    ping().


fib(N) -> fib(N, 0, 1).

fib(0, N1, _) -> N1;
fib(N, N1, N2) ->
    fib(N-1, N2, N1 + N2).


fibserver() ->
    receive
        {fib, N, Pid} when is_pid(Pid) ->
            spawn(fun () ->
                          Pid ! {fib, self(), N, fib(N)}
                  end
                 );
        M ->
            io:format("Got an invalid message: ~p~n", [M])
    end,
    fibserver().

init_numberserver() ->
    numberserver(0, 0, 0).

numberserver(High, Puts, Gets) ->
    receive
        {put, N} when is_integer(N) ->
            numberserver(max(High, N), Puts + 1, Gets);
        {query, Pid} when is_pid(Pid) ->
            Pid ! {largest, High},
            numberserver(High, Puts, Gets + 1);
        {statistics, Pid} when is_pid(Pid) ->
            Pid ! {Puts, Gets},
            numberserver(High, Puts, Gets)
    end,
    numberserver(High, Puts, Gets).

n_connected_cells(0, Pid) ->
    io:format("~p ~p~n", [Pid, 0]),
    forward_cell(Pid);
n_connected_cells(N, Pid) when N > 0 ->
    Pi = spawn(fun () ->
                       forward_cell(n_connected_cells(N-1, Pid))
            end),
    io:format("~p ~p~n", [Pi, N]),
    Pi.

forward_cell(Child) ->
    receive X -> Child ! X end,
    forward_cell(Child).
