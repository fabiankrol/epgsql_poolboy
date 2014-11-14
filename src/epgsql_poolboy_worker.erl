-module(epgsql_poolboy_worker).


-export([code_change/3]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([init/1]).
-export([start_link/1]).
-export([terminate/2]).

-behaviour(poolboy_worker).
-behaviour(gen_server).

-record(state, {conn :: pid()}).


start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

connect(Host, undefined, Opts) ->
    pgsql:connect(Host, Opts);
connect(Host, Password, Opts) ->
    pgsql:connect(Host, Password, Opts).

init(Args) ->
    Host = proplists:get_value(host, Args, "localhost"),
    Password = proplists:get_value(password, Args),
    ConnOpts = proplists:get_value(opts, Args, []),
    {ok, Pid} = connect(Host, Password, ConnOpts),
    {ok, #state{conn=Pid}}.


handle_call({F, A1, A2, A3, A4, A5, A6}, _From, State=#state{conn=Conn}) ->
    Reply = pgsql:F(Conn, A1, A2, A3, A4, A5, A6),
    handle_call_reply(Reply, State);
handle_call({F, A1, A2, A3, A4, A5}, _From, State=#state{conn=Conn}) ->
    Reply = pgsql:F(Conn, A1, A2, A3, A4, A5),
    handle_call_reply(Reply, State);
handle_call({F, A1, A2, A3, A4}, _From, State=#state{conn=Conn}) ->
    Reply = pgsql:F(Conn, A1, A2, A3, A4),
    handle_call_reply(Reply, State);
handle_call({F, A1, A2, A3}, _From, State=#state{conn=Conn}) ->
    Reply = pgsql:F(Conn, A1, A2, A3),
    handle_call_reply(Reply, State);
handle_call({F, A1, A2}, _From, State=#state{conn=Conn}) ->
    Reply = pgsql:F(Conn, A1, A2),
    handle_call_reply(Reply, State);
handle_call({F, A1}, _From, State=#state{conn=Conn}) ->
    Reply = pgsql:F(Conn, A1),
    handle_call_reply(Reply, State);
handle_call(F, _From, State=#state{conn=Conn}) ->
    Reply = pgsql:F(Conn),
    handle_call_reply(Reply, State).

handle_cast(_Msg, State) ->
    {stop, unhandled_cast, State}.

terminate(_Reason, #state{conn=Conn}) ->
    ok = pgsql:close(Conn).

handle_call_reply(Reply, State) ->
    {reply, Reply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_, State) ->
    {noreply, State}.
