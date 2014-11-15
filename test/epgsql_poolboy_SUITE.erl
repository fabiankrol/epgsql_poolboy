-module(epgsql_poolboy_SUITE).


-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() ->
    [simple_connect, transaction].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(epgsql_poolboy),
    Config.

end_per_suite(_Config) ->
    application:stop(epgsql_poolboy).

create_pool(PoolName) ->
    SizeArgs = [{size, 10},
                {max_overflow, 20}],

    WorkerArgs = [{hostname, "localhost"},
                  {opts, [{database, "epgsql_test_database"}]}],

    epgsql_poolboy:start_pool(PoolName, SizeArgs, WorkerArgs).

simple_connect(_Config) ->
    PoolName = postgres_pool,
    {ok, Pid} = create_pool(PoolName),
    true = is_process_alive(Pid),
    ok = epgsql_poolboy:stop_pool(PoolName),
    false = is_process_alive(Pid).

transaction(_Config) ->
    PoolName = postgres_pool,
    {ok, _} = create_pool(PoolName),
    InTransaction =
        fun(C) ->
                {ok, _, Rows} = pgsql:equery(C, "SELECT * FROM test_database"),
                Next = lists:max([N || {N} <- Rows]) + 1,
                {ok,    1} = pgsql:equery(C, "INSERT INTO test_database VALUES($1)", [Next])
        end,

    epgsql_poolboy:with_transaction(PoolName, InTransaction).
