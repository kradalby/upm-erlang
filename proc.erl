-module(proc).

-compile(export_all).


loop() ->
    receive
        {Message, PID} -> io:format("~p~n", [Message]), PID ! received
    end,
    loop().
