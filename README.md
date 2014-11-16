# epgsql poolboy client

About
=========

epgsql connection pool using poolboy

Usage
=========

```erlang
SizeArgs = [{size, 10},
            {max_overflow, 20}],
WorkerArgs = [{host, "127.0.0.1"},
              {opts, [{database, "some_database"}
                      {port, 8087}]}],
PoolName = badger_pool,
epgsql_poolboy:start_pool(PoolName, SizeArgs, WorkerArgs),

{ok, [ColSpecs, Rows]} = epgsql_poolboy:equery(PoolName, "SELECT * FROM some_database")

epgsql_poolboy:stop_pool(PoolName).
```

TODO
=========

* Tests

