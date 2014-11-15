-module(epgsql_poolboy_SUITE).


-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() ->
    [simple_connect].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(epgsql_poolboy),
    Config.

end_per_suite(_Config) ->
    application:stop(epgsql_poolboy).

simple_connect(_Config) ->
    SizeArgs = [{size, 10},
                {max_overflow, 20}],

    WorkerArgs = [{hostname, "localhost"},
                  {opts, []}],

    PoolName = postgres_pool,
    {ok, Pid} = epgsql_poolboy:start_pool(PoolName, SizeArgs, WorkerArgs),
    true = is_process_alive(Pid),

    ok = epgsql_poolboy:stop_pool(PoolName),
    false = is_process_alive(Pid).
