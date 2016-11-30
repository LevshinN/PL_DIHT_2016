-module(transactions_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([terminate/2]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(amount, {sum}).

start_link() ->
    gen_server:start_link({global, transactions_server}, transactions_server, [], []).

load_data(Key1) ->
	if 
		Key1 == '$end_of_table' -> [];
		true -> [dets:lookup("transactions.back", Key1)] ++ load_data(dets:next("transactions.back", Key1))
	end. 

print([])-> [];
print([H|T]) ->
    io:format("printing: ~p~n", [H]),
    [H|print(T)].
            
configure_history(Data) ->
	SortedData = lists:sort(Data),
	[Amount || {Timestamp, Amount} <- Data].

init(_Args) ->
	dets:open_file("transactions.back", []),
	Data = load_data(dets:first("transactions.back")),
	Operations = configure_history(Data),
	print(Operations),
    	{ok, Operations}.

terminate(_, _) ->
	io:format("bye - bye ~n", []),
	dets:sync("transactions.back"),
	ok.

now_us({MegaSecs,Secs,MicroSecs}) ->
	(MegaSecs*1000000 + Secs)*1000000 + MicroSecs.
               

handle_call(history, _, Operations) ->
	io:format("History ~p~n", [length(Operations)]),
	print(Operations),
	{reply, Operations, Operations};

handle_call(_, _, Operations) -> 
	{reply, 'No such command', Operations}.

handle_cast({widthdraw, Amount}, Operations) ->
	timer:sleep(1),
	Timestamp = now_us(erlang:timestamp()),
	dets:insert("transactions.back", {Timestamp, Amount}), 
	{noreply, Operations ++ [Amount]};

handle_cast(clear, Operations) ->            
	dets:delete_all_objects("transactions.back"),
	{noreply, []};

handle_cast(_, Operations) -> {noreply, Operations}.

	