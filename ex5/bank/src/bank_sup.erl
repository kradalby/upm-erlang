-module(bank_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    {ok, {{one_for_one, 5, 60},
        [{bank_instance,
            {bank_serv, start_link, [[]]},
            permanent,
            5000,
            worker,
            [bank_serv]}
        ]}
    }.
