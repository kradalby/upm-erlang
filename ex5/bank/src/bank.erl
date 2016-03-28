-module(bank).
-behaviour(application).

-export([
    start/2,
    stop/1
]).

-export([
    new/1,
    withdraw/2,
    deposit/2,
    transfer/3,
    balance/1,
    information/0,
    fail/0
]).

-define(SERVERNAME, bank_serv).

-spec start('normal',_) -> {'ok', pid()}.
start(normal, _Args) ->
    bank_sup:start_link().

-spec stop(_) -> 'ok'.
stop(_State) ->
    ok.

-spec new(number()) -> boolean().
new(AccountNumber) ->
    gen_server:call(?SERVERNAME, {new, AccountNumber}).

-spec withdraw(number(),number()) -> number().
withdraw(AccountNumber, Amount) ->
    gen_server:call(?SERVERNAME, {withdraw, AccountNumber, Amount}).

-spec deposit(number(),number()) -> number().
deposit(AccountNumber, Amount) ->
    gen_server:call(?SERVERNAME, {deposit, AccountNumber, Amount}).

-spec transfer(number(),number(),number()) -> number().
transfer(FromAccount, ToAccount, Amount) ->
    gen_server:call(?SERVERNAME, {transfer, FromAccount, ToAccount, Amount}).

-spec balance(number()) -> number().
balance(AccountNumber) ->
    gen_server:call(?SERVERNAME, {balance, AccountNumber}).

-spec information() -> list().
information() ->
    gen_server:call(?SERVERNAME, {info}).


-spec fail() -> 'ok'.
fail() ->
    gen_server:cast(?SERVERNAME, {fail}).
