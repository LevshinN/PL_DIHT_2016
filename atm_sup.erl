-module(atm_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(atm_sup, []).

init(_Args) ->
	SupFlags = #{strategy => one_for_one, intensity => 10, period => 3},
	ChildSpecs = [#{id => atm_server,
                    start => {atm_server, start_link, []},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [atm_server]},
			#{id => transactions_server,
                    start => {transactions_server, start_link, []},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [transactions_server]}],
    {ok, {SupFlags, ChildSpecs}}.
