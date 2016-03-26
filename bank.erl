-module(bank).

-export([
         create_bank/0,
         new_account/2,
         withdraw_money/3,
         deposit_money/3,
         transfer/4,
         get_balance/2,
         account_information/1
        ]).


create_bank() ->
    spawn(fun () -> bank_instance([]) end).

new_account(Bank, AccountNumber) ->
    Bank ! {new, AccountNumber, self()}.

withdraw_money(Bank, AccountNumber, Amount) ->
    Bank ! {withdraw, AccountNumber, Amount, self()},
    receive
        {withdraw, N} -> N
    end.


deposit_money(Bank, AccountNumber, Amount) ->
    Bank ! {deposit, AccountNumber, Amount, self()}.

transfer(Bank, FromAccount, ToAccount, Amount) ->
    Bank ! {transfer, FromAccount, ToAccount, Amount, self()}.

get_balance(Bank, AccountNumber) ->
    Bank ! {balance, AccountNumber, self()}.

account_information(Bank) ->
    Bank ! {info, self()}.

% Accounts = [{AccountNumber, Amount}]

bank_instance(Accounts) ->
    receive
        {new, AccountNumber, Pid} -> bank_instance(new(Accounts, AccountNumber, Pid));
        {withdraw, AccountNumber, Amount, Pid} ->
            {NewAccounts, NewAmount} = withdraw(Accounts, AccountNumber, Amount),
            Pid ! {withdraw, NewAmount},
            bank_instance(NewAccounts);
        {deposit, AccountNumber, Amount, Pid} ->
            {NewAccounts, NewAmount} = deposit(Accounts, AccountNumber, Amount),
            Pid ! NewAmount,
            bank_instance(NewAccounts);
        {transfer, FromAccount, ToAccount, Amount, Pid} ->
            {NewAccounts, NewAmount} = internal_transfer(Accounts, FromAccount, ToAccount, Amount),
            Pid ! NewAmount,
            bank_instance(NewAccounts);
        {balance, AccountNumber, Pid} ->
            Pid ! balance(Accounts, AccountNumber);
        {info, Pid} ->
            Pid ! Accounts
    end,
    bank_instance(Accounts).

new(Accounts, AccountNumber, Pid) ->
    case lists:keyfind(AccountNumber, 1, Accounts) of
        false -> Pid ! true, [{AccountNumber, 0}|Accounts];
        {AccountNumber, Amount} -> Pid ! false,  Accounts
    end.

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

internal_transfer(Accounts, FromAccount, ToAccount, Amount) ->
    case withdraw(Accounts, FromAccount, Amount) of
        {NewAccounts, 0} -> {Accounts, 0};
        {NewAccounts, NewAmount} ->
            {NewAccounts2, _} = deposit(NewAccounts, ToAccount, Amount),
            {NewAccounts2, Amount}
    end.

balance(Accounts, AccountNumber) ->
    case lists:keyfind(AccountNumber, 1, Accounts) of
        false -> 0;
        {AccountNumber, Amount} -> Amount
    end.
