-module(epgsql_pool_status).

-behaviour(gen_server).

-export([code_change/3]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([init/1]).
-export([start_link/1]).
-export([terminate/2]).


-record(state, {name :: atom(),
                ref :: reference(),
                timer :: timer:tref()}).

start_link(Name) when is_atom(Name) ->
    gen_server:start_link(?MODULE, [Name], []).

init([Name]) when is_atom(Name) ->
    Ref = make_ref(),
    {ok, Tref} = timer:send_interval(1000, self(), {status, Ref}),
    process_flag(trap_exit, true),
    {ok, #state{name=Name, ref=Ref, timer=Tref}}.

handle_call(_Msg, _From, State) ->
    {stop, unhandled_call, State}.

handle_cast(_Msg, State) ->
    {stop, unhandled_cast, State}.

handle_info({status, Ref}, State = #state{name=Name, ref=Ref}) ->
    try
        {_StatusName, WorkerQueueLen, Overflow, MonitorsSize} = poolboy:status(Name),
        WorkerGauge   = metric_name(Name, "workers"),
        OverflowGauge = metric_name(Name, "overflow"),
        MonitorsGauge = metric_name(Name, "monitors"),
        ok = quintana:notify_gauge({WorkerGauge, WorkerQueueLen}),
        ok = quintana:notify_gauge({OverflowGauge, Overflow}),
        ok = quintana:notify_gauge({MonitorsGauge, MonitorsSize})
    catch
        timeout:{gen_server, call, [_, status]} ->
            Timeout = metric_name(Name, "stats_timeout"),
            ok = quintana:notify_spiral({Timeout, 1})
    end,
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

metric_name(PoolName, StatName) ->
    NameString = atom_to_list(PoolName),
    list_to_binary(NameString ++ ".pool." ++ StatName).
