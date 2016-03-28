-module(bank_serv).
-behaviour(gen_server).


-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).

-export([
    start_link/1,
    stop/0
]).


-spec init([]) -> {'ok',[]}.
init([]) ->
    {ok, []}.

-spec terminate(_,list()) -> 'ok'.
terminate(_Reason, _State) ->
    ok.

-spec handle_call(any(),tuple(),list()) -> {'noreply',_} | {'reply',_,_}.
handle_call({new, AccountNumber}, _From, State) ->
    {NewState, Response} = new(State, AccountNumber),
    {reply, Response, NewState};

handle_call({withdraw, AccountNumber, Amount}, _From, State) ->
    {NewState, Response} = withdraw(State, AccountNumber, Amount),
    {reply, Response, NewState};

handle_call({deposit, AccountNumber, Amount}, _From, State) ->
    {NewState, Response} = deposit(State, AccountNumber, Amount),
    {reply, Response, NewState};

handle_call({transfer, FromAccount, ToAccount, Amount}, _From, State) ->
    {NewState, Response} = transfer(State, FromAccount, ToAccount, Amount),
    {reply, Response, NewState};

handle_call({balance, AccountNumber}, _From, State) ->
    Response = balance(State, AccountNumber),
    {reply, Response, State};

handle_call({info}, _From, State) ->
    {reply, State, State};

handle_call(_Pattern, From, State) ->
    io:format("Received bad call from: ~p~n", [From]),
    {noreply, State}.

-spec handle_cast(any(),list()) -> {'noreply',_}.
handle_cast({fail}, State) ->
    2/0,
    {noreply, State};

handle_cast(_Pattern, State) ->
    io:format("Received bad cast"),
    {noreply, State}.

-spec handle_info(_,list()) -> {'noreply',_}.
handle_info(Info, State) ->
    io:format("Unexpected message: ~p~n", [Info]),
    {noreply, State}.

-spec code_change(_,list(),_) -> {'ok',_}.
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.


-spec start_link([]) -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link([]) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> 'ok' | {'ok',_}.
stop() ->
    gen_server:stop(?MODULE).


-spec new(maybe_improper_list(),number()) -> {maybe_improper_list(),boolean()}.
new(Accounts, AccountNumber) ->
    case lists:keyfind(AccountNumber, 1, Accounts) of
        false -> {[{AccountNumber, 0}|Accounts], true};
        {AccountNumber, _Amount} -> {Accounts, false}
    end.

-spec withdraw(maybe_improper_list(),number(),number()) -> {maybe_improper_list(),number()}.
withdraw(Accounts, AccountNumber, Amount) ->
    case lists:keyfind(AccountNumber, 1, Accounts) of
        false -> {Accounts, 0};
        {AccountNumber, CurrentAmount} ->
            if Amount > CurrentAmount -> {Accounts, 0};
               Amount =< CurrentAmount ->
                    NewAccounts = lists:keyreplace(
                     AccountNumber,
                     1,
                     Accounts,
                     {AccountNumber, CurrentAmount - Amount}
                    ),
                    {NewAccounts, Amount}
            end
        end.

-spec deposit(maybe_improper_list(),number(),number()) -> {maybe_improper_list(),number()}.
deposit(Accounts, AccountNumber, Amount) ->
    case lists:keyfind(AccountNumber, 1, Accounts) of
        false -> {Accounts, 0};
        {AccountNumber, CurrentAmount} ->
               NewAccounts = lists:keyreplace(
                 AccountNumber,
                 1,
                 Accounts,
                 {AccountNumber, CurrentAmount + Amount}
                ),
               {NewAccounts, CurrentAmount + Amount}
        end.

-spec transfer(maybe_improper_list(),number(),number(),number()) -> {maybe_improper_list(),_}.
transfer(Accounts, FromAccount, ToAccount, Amount) ->
    case withdraw(Accounts, FromAccount, Amount) of
        {_NewAccounts, 0} -> {Accounts, 0};
        {NewAccounts, _NewAmount} ->
            {NewAccounts2, _} = deposit(NewAccounts, ToAccount, Amount),
            {NewAccounts2, Amount}
    end.

-spec balance(maybe_improper_list(),number()) -> number().
balance(Accounts, AccountNumber) ->
    case lists:keyfind(AccountNumber, 1, Accounts) of
        false -> 0;
        {AccountNumber, Amount} -> Amount
    end.
