-module(kabue_health).

-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-export([
        lookup/0
    ]).

-export_type([
        state/0
    ]).

-type state() :: #{
        status => kabue_status:status()
      , is_first_check := boolean()
    }.


start_link() ->
    %% Start the gen_server and immediately schedule the first health check
    %% tick.  We avoid spawning a separate process; instead we rely on
    %% send_after to deliver the `tick` message to ourselves.
    Return = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    _ = erlang:send_after(0, ?MODULE, tick),
    Return.

init(_Settings) ->
    process_flag(trap_exit, true),
    State = #{
        is_first_check => true
    },
    {ok, State}.

handle_call(lookup_status, _From, State) ->
    {reply, klsn_map:lookup([status], State), State}.

handle_cast(main_loop, State) ->
    %% Retained for backward compatibility; simply forward to tick so callers
    %% that might still cast(main_loop) keep working.
    self() ! tick,
    {noreply, State};
handle_cast({upsert_status, Status}, State0) ->
    State = State0#{
        status => Status
    },
    {noreply, State}.

handle_info({'EXIT', _, normal}, State) ->
    {noreply, State};
handle_info(tick, State0) ->
    %% Periodic health-check execution.  We run the same logic that used to be
    %% inside the spawned processes, but keep it within this process to avoid
    %% unbounded process growth.  The work itself is lightweight and runs once
    %% per minute.
    Status = kabue_status:status(),
    State10 = State0#{status => Status, is_first_check => false},

    %% Trigger user-defined command on health check failure (same condition as
    %% before).
    case {Status, maps:get(is_first_check, State0)} of
        {#{health := false}, false} ->
            on_health_check_failure();
        _ -> ok
    end,

    %% Kick the Rakuten RSS market self-healing logic.
    case klsn_map:get([kabue_rakuten_rss_market, time_since_update], Status) div 60 of
        Time when Time > 50, Time rem 2 =:= 0 ->
            {ok, Ticker} = application:get_env(kabue, rakuten_rss_health_check_ticker),
            kabue_rakuten_rss_market:add(Ticker);
        Time when Time > 50, Time rem 2 =:= 1 ->
            {ok, Ticker} = application:get_env(kabue, rakuten_rss_health_check_ticker),
            kabue_rakuten_rss_market:remove(Ticker);
        _ -> ok
    end,

    %% Reschedule next tick.
    _ = erlang:send_after(60 * 1000, self(), tick),
    {noreply, State10};
handle_info(Info, State) ->
    logger:info("~p:~p/~p: line=~p info=~p", [
        ?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE, Info]),
    {noreply, State}.

terminate(_reason, _State) ->
    ok.

-spec lookup() -> klsn:maybe(kabue_status:status()).
lookup() ->
    gen_server:call(?MODULE, lookup_status).


on_health_check_failure() ->
    case application:get_env(kabue, on_health_check_failure) of
        {ok, {value, Cmd}} ->
            os:cmd(Cmd);
        _ ->
            ok
    end.

