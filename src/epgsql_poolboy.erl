-module(epgsql_poolboy).

-export([bind/3]).
-export([bind/4]).
-export([equery/2]).
-export([equery/3]).
-export([execute/2]).
-export([execute/3]).
-export([execute/4]).
-export([parse/2]).
-export([parse/3]).
-export([parse/4]).
-export([squery/2]).
-export([start_pool/3]).
-export([stop_pool/1]).
-export([sync/1]).
-export([with_transaction/2]).

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

equery(PoolNameOrAtom, Sql) ->
    equery(PoolNameOrAtom, Sql, []).

equery(PoolNameOrAtom, Sql, []) ->
    exec(PoolNameOrAtom, {equery, [Sql, []]}).

squery(PoolNameOrAtom, Sql) ->
    exec(PoolNameOrAtom, {squery, [Sql]}).

parse(PoolNameOrAtom, Sql) ->
    parse(PoolNameOrAtom, "", Sql, []).

parse(PoolNameOrAtom, Sql, Types) ->
    parse(PoolNameOrAtom, "", Sql, Types).

parse(PoolNameOrAtom, Name, Sql, Types) ->
    exec(PoolNameOrAtom, {parse, [PoolNameOrAtom, Name, Sql, Types]}).

bind(PoolNameOrAtom, Statement, Parameters) ->
    bind(PoolNameOrAtom, Statement, "", Parameters).

bind(PoolNameOrAtom, Statement, PortalName, Parameters) ->
    exec(PoolNameOrAtom, {bind, [Statement, PortalName, Parameters]}).

execute(PoolNameOrAtom, S) ->
    execute(PoolNameOrAtom, S, "", 0).

execute(PoolNameOrAtom, S, N) ->
    execute(PoolNameOrAtom, S, "", N).

execute(PoolNameOrAtom, S, PortalName, N) ->
    exec(PoolNameOrAtom, {execute, [S, PortalName, N]}).

sync(PoolNameOrAtom) ->
    exec(PoolNameOrAtom, sync).

with_transaction(PoolNameOrAtom, F) ->
    exec(PoolNameOrAtom, {with_transaction, [F]}).

exec(Worker, Args) when is_pid(Worker) ->
    gen_server:call(Worker, Args);
exec(PoolName, Args) ->
    PoolNameBin = atom_to_binary(PoolName, latin1),
    BaseMetricName = [<<"epgsql_poolboy.">>, PoolNameBin, stat(Args)],
    NameTimer = list_to_binary(BaseMetricName),
    NameSpiral = list_to_binary([<<"epgsql_poolboy.">>, PoolNameBin, stat(Args), ".counter"]),
    Fun = fun(Worker) ->
                  Metric = quintana:begin_timed(NameTimer),
                  Res = gen_server:call(Worker, Args),
                  ok = quintana:notify_timed(Metric),
                  ok = quintana:notify_spiral({NameSpiral, 1}),
                  Res
          end,
    poolboy:transaction(PoolName, Fun).

stat({F, _}) when is_atom(F) ->
    append_stat(F);
stat(F) when is_atom(F) ->
    append_stat(F).

append_stat(F) when is_atom(F) ->
    BinFuncName = atom_to_binary(F, latin1),
    <<".", BinFuncName/binary>>.
