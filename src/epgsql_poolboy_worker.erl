-module(epgsql_poolboy_worker).


-export([code_change/3]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([init/1]).
-export([start_link/1]).
-export([terminate/2]).

-define(TIMEOUT, 30000).
-define(CONNECTION_ERROR_NOTIFY, <<"epgsql_poolboy.connection_error">>).

-behaviour(poolboy_worker).
-behaviour(gen_server).

-record(state, {conn :: pid(),
                host :: string(),
                username :: string(),
                password :: string(),
                opts :: proplists:proplist()}).


start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

connect(Host, undefined, undefined, Opts) ->
    pgsql:connect(Host, Opts);
connect(Host, Username, undefined, Opts) ->
    pgsql:connect(Host, Username, Opts);
connect(Host, Username, Password, Opts) ->
    pgsql:connect(Host, Username, Password, Opts).

try_connect(#state{conn=undefined, host=Host, username=Username,
                   password=Password, opts=Opts}) ->
    try
        connect(Host, Username, Password, Opts)
    catch
        error:{{badmatch, econnrefused = Reason}, _} ->
            ok = notify_connection_error(),
            {error, Reason}
    end.

init(Args) ->
    Host = proplists:get_value(host, Args, "localhost"),
    Username = proplists:get_value(username, Args),
    Password = proplists:get_value(password, Args),
    ConnOpts = proplists:get_value(opts, Args, []),
    BaseState =
        #state{host=Host, username=Username, password=Password,
               opts=ConnOpts},
    case try_connect(BaseState) of
        {ok, Pid} ->
            {ok, BaseState#state{conn=Pid}};
        {error, R} ->
            error_logger:error_msg("Error connecting to postgresql: ~p", [R]),
            erlang:send_after(?TIMEOUT, self(), reconnect),
            {ok, BaseState}
    end.

handle_call(F, _From, State = #state{conn=undefined}) ->
    case try_connect(State) of
        {ok, Conn} ->
            Resp = mapply(F, Conn),
            handle_call_reply(Resp, State#state{conn=Conn});
        {error, R} ->
            error_logger:error_msg("Error calling postgresql: ~p", [R]),
            Reply = {error, not_connected},
            {reply, Reply, State, ?TIMEOUT}
    end;
handle_call(F, _From, State=#state{conn=Conn}) ->
    Reply = mapply(F, Conn),
    handle_call_reply(Reply, State).

handle_cast(_Msg, State) ->
    {stop, unhandled_cast, State}.

terminate(_Reason, #state{conn=Conn}) ->
    ok = pgsql:close(Conn).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(timeout, State = #state{conn=undefined, host=Host}) ->
    case try_connect(State) of
        {ok, Pid} ->
            {noreply, State#state{conn=Pid}};
        {error, _} ->
            error_logger:info_msg("Postgres connection reconnecting: ~s", [Host]),
            {noreply, State, ?TIMEOUT}
    end;
handle_info(_, State) ->
    {noreply, State}.

%% Internal

mapply(F, Conn) when is_atom(F) ->
    pgsql:F(Conn);
mapply({F, Args}, Conn) ->
    apply(pgsql, F, [Conn|Args]).

handle_call_reply(Reply, State) ->
    {reply, Reply, State}.

notify_connection_error() ->
    ok = epgsql_metrics:notify_histogram(?CONNECTION_ERROR_NOTIFY).
