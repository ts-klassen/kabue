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
    Return = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    gen_server:cast(?MODULE, main_loop),
    Return.

init(_Settings) ->
    process_flag(trap_exit, true),
    State = #{
        is_first_check => true
    },
    {ok, State}.

handle_call(lookup_status, _From, State) ->
    {reply, klsn_map:lookup([status], State), State}.

handle_cast(main_loop, State0) ->
    spawn_link(fun()->
        timer:sleep(60 * 1000),
        gen_server:cast(?MODULE, main_loop)
    end),
    spawn_link(fun()->
        Status = kabue_status:status(),
        gen_server:cast(?MODULE, {upsert_status, Status}),
        case {Status, maps:get(is_first_check, State0)} of
            {#{health := false}, false} ->
                on_health_check_failure();
            _ ->
                ok
        end,
        case
            klsn_map:get([kabue_rakuten_rss_market, time_since_update]
                       , Status) div 60
        of
            Time when Time > 50, Time rem 2 =:= 0 ->
                {ok, Ticker} = application:get_env(
                    kabue, rakuten_rss_health_check_ticker),
                kabue_rakuten_rss_market:add(Ticker);
            Time when Time > 50, Time rem 2 =:= 1 ->
                {ok, Ticker} = application:get_env(
                    kabue, rakuten_rss_health_check_ticker),
                kabue_rakuten_rss_market:remove(Ticker);
            _ ->
                ok
        end
    end),
    State = State0#{
        is_first_check => false
    },
    {noreply, State};
handle_cast({upsert_status, Status}, State0) ->
    State = State0#{
        status => Status
    },
    {noreply, State}.

handle_info({'EXIT', _, normal}, State) ->
    {noreply, State};
handle_info(Info, State) ->
    error_logger:info_msg("function=~p:~p/~p, line=~p~ninfo=~p", [
            ?MODULE
          , ?FUNCTION_NAME
          , ?FUNCTION_ARITY
          , ?LINE
          , Info
        ]),
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

