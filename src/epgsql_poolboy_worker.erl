-module(epgsql_poolboy_worker).

-export([init/1]).
-export([start_link/1]).

-behaviour(poolboy_worker).

-record(state, {conn :: pid()}).


start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(_) ->
    {ok, #state{}}.
