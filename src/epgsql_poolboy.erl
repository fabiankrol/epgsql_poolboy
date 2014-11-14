-module(epgsql_poolboy).


-export([equery/2]).
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

equery(PoolName, Sql) ->
    exec(PoolName, {equery, Sql}).

exec(PoolName, Args) ->
   PoolNameBin = atom_to_binary(PoolName, latin1),
   Name = list_to_binary([<<"epgsql_poolboy.">>, PoolNameBin]),
    %%TODO: Steal puzza's fancy pants metric on the arguments: stat(Args)])
    Fun = fun(Worker) ->
                  Metric = folsom_metrics:histogram_timed_begin(Name),
                  Res = gen_server:call(Worker, Args),
                  ok = histogram_timed_notify(Metric),
                  Res
          end,
    poolboy:transaction(PoolName, Fun).

histogram_timed_notify({Name, _} = Metric) ->
     try
         ok = folsom_metrics:histogram_timed_notify(Metric)
     catch _:_ ->
             folsom_metrics:new_histogram(Name),
             folsom_metrics:safely_histogram_timed_notify(Metric)
     end.
