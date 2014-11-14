-module(epgsql_poolboy).

-export([stop_pool/1]).
-export([start_pool/3]).

-include("defaults.hrl").


-spec
start_pool(atom(), list(term()), list(term())) -> 
    supervisor:startlink_ret().
start_pool(Name, SizeArgs, WorkerArgs) ->
    epgsql_poolboy_sup:start_pool(Name, SizeArgs, WorkerArgs).

-spec
stop_pool(atom()) ->
    ok | stop_return().
stop_pool(Name) ->
    epgsql_poolboy_sup:stop_pool(Name).
