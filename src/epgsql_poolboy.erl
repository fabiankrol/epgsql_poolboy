-module(epgsql_poolboy).

-export([start_pool/3]).


-spec
start_pool(atom(), list(term()), list(term())) -> 
    supervisor:startlink_ret().
start_pool(Name, SizeArgs, WorkerArgs) ->
    epgsql_poolboy_sup:start_pool(Name, SizeArgs, WorkerArgs).
