-module(atm_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([terminate/2]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link() ->
    gen_server:start_link({global, atm_server}, atm_server, [], []).

init(_Args) ->
    {ok, [5000, 50, 50, 50, 1000, 5000, 1000, 500, 100]}.

terminate(_, _) -> 'ok'.

handle_call({widthdraw, Amount}, _, Banknotes) ->
	try atm:widthdraw(Amount, Banknotes) of
		{Response, GiveBanknotes, RestBanknotes} ->
			if
				Response == ok ->
					gen_server:cast({global, transactions_server}, {widthdraw, Amount});
				true -> true
			end,
			{reply, {Response, GiveBanknotes}, RestBanknotes}
	catch
		_:_ -> {stop, normal, Banknotes}
    	end;

handle_call(_, _, Banknotes) -> {reply, 'No such command',Banknotes}.

handle_cast(_, Banknotes) -> {noreply, Banknotes}.
