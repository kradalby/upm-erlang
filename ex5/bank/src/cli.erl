-module(cli).

-export([
    new/1,
    withdraw/2,
    deposit/2,
    transfer/3,
    balance/1,
    information/0,
    fail/0
]).

-define(SERVERNAME, bank).

new(AccountNumber) ->
    gen_server:call(?SERVERNAME, {new, AccountNumber}).

withdraw(AccountNumber, Amount) ->
    gen_server:call(?SERVERNAME, {withdraw, AccountNumber, Amount}).

deposit(AccountNumber, Amount) ->
    gen_server:call(?SERVERNAME, {deposit, AccountNumber, Amount}).

transfer(FromAccount, ToAccount, Amount) ->
    gen_server:call(?SERVERNAME, {transfer, FromAccount, ToAccount, Amount}).

balance(AccountNumber) ->
    gen_server:call(?SERVERNAME, {balance, AccountNumber}).

information() ->
    gen_server:call(?SERVERNAME, {info}).


fail() ->
    gen_server:cast(?SERVERNAME, {fail}).
