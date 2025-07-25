-module(kabue_mufje_ws_apic).

-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-export([
        is_connected/1
      , last_updated_at/1
      , last_websocket_at/1
      , data_size/1
      , lookup/2
      , subscribe_signal/2
      , unsubscribe_signal/2
      , await_signal/2
      , flush_signal/1
    ]).

-export_type([
        state/0
      , mode/0
      , data/0
    ]).

-type state() :: #{
        mode() := #{
            pid := pid()
          , stream_ref := klsn:maybe(reference())
          , is_upgraded := boolean()
          , last_updated_at := klsn:maybe(klsn_flux:timestamp())
          , last_websocket_at := klsn:maybe(klsn_flux:timestamp())
          , data := #{
                kabue_mufje_rest_apic:ticker() => data()
            }
          , signal := #{
                kabue_mufje_rest_apic:ticker() => [pid()]
            }
        }
    }.


-type mode() :: real | test.

-type data() :: #{
                    previous | current => #{
                        hash := klsn:binstr()
                      , board := kabue_mufje_types:board()
                      , timestamp := klsn_flux:timestamp()
                    }
                }.


-type signal() :: {update, mode(), kabue_mufje_rest_apic:ticker()}
                | terminate
                .


-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init(nil()) -> {ok, state()}.
init(_Options) ->
    RetryMax = 180,
    GunOpts = #{
        supervise => true
      , retry => RetryMax
      , retry_fun => fun(Cnt, _) ->
            case Cnt of
                1 ->
                    gen_server:cast(?MODULE, too_many_retry),
                    #{retries => 0, timeout => 0};
                _ ->
                    Stage = RetryMax - Cnt,
                    Sleep = round(1000 * rand:uniform() + math:exp(Stage)),
                    timer:sleep(min(1000*60, Sleep)),
                    gen_server:cast(?MODULE, {retry, Cnt}),
                    #{retries => Cnt-1, timeout => 1000}
            end
        end
    },
    process_flag(trap_exit, true),
    {ok, RealPid} = gun:open("localhost", 18080, GunOpts),
    ModeBase = #{
        is_upgraded => false
      , last_updated_at => none
      , last_websocket_at => none
      , data => #{}
      , signal => #{}
    },
    Real = ModeBase#{
        pid => RealPid
    },
    {ok, TestPid} = gun:open("localhost", 18081, GunOpts),
    Test = ModeBase#{
        pid => TestPid
    },
    State = #{
        real => Real
      , test => Test
    },
    {ok, State}.

handle_call({data_size, Mode}, _From, State) ->
    {reply, maps:size(klsn_map:get([Mode, data], State)), State};
handle_call({lookup_from_state, Path}, _From, State) ->
    {reply, klsn_map:lookup(Path, State), State}.

handle_cast({retry, _Retry}, State) ->
    io:format("retry: ~p~n", [_Retry]),
    {noreply, State};
handle_cast({subscribe_signal, Mode, Ticker, Pid}, State0) ->
    CurrentList = klsn_map:get([Mode, signal, Ticker], State0, []),
    RemovedList = lists:delete(Pid, CurrentList),
    List = [Pid | RemovedList],
    State = klsn_map:upsert([Mode, signal, Ticker], List, State0),
    {noreply, State};
handle_cast({unsubscribe_signal, Mode, Ticker, Pid}, State0) ->
    CurrentList = klsn_map:get([Mode, signal, Ticker], State0, []),
    RemovedList = lists:delete(Pid, CurrentList),
    State = klsn_map:upsert([Mode, signal, Ticker], RemovedList, State0),
    {noreply, State};
handle_cast(too_many_retry, State) ->
    {stop, too_many_retry, State}.

handle_info({gun_upgrade, _Pid, Ref, _, _}, State0) ->
    io:format("gun_upgrade~n"),
    Timestamp = klsn_flux:timestamp(),
    Mode = mode_from_ref(Ref, State0),
    State10 = klsn_map:upsert([Mode, is_upgraded], true, State0),
    State20 = klsn_map:upsert([Mode, last_websocket_at], {value, Timestamp}, State10),
    {noreply, State20};
handle_info({gun_down, _Pid, Proto, Reason, [Ref]}, State0)
    when (Proto =:= ws orelse Proto =:= http) andalso
         (Reason =:= closed orelse Reason =:= normal) ->
    io:format("gun_down~n"),
    Mode = mode_from_ref(Ref, State0),
    State10 = klsn_map:upsert([Mode, is_upgraded], false, State0),
    {noreply, State10};
handle_info({gun_ws, _Pid, Ref, close}, State0) ->
    io:format("gun_ws close~n"),
    Mode = mode_from_ref(Ref, State0),
    State10 = klsn_map:upsert([Mode, is_upgraded], false, State0),
    {noreply, State10};
handle_info({gun_error, _Pid, Ref, closed}, State0) ->
    io:format("gun_error closed~n"),
    Mode = mode_from_ref(Ref, State0),
    State10 = klsn_map:upsert([Mode, is_upgraded], false, State0),
    {noreply, State10};
handle_info({gun_up, Pid, http}, State0) ->
    io:format("gun_up~n"),
    Timestamp = klsn_flux:timestamp(),
    Mode = mode_from_pid(Pid, State0),
    StreamRef = gun:ws_upgrade(Pid, "/kabusapi/websocket", []),
    State10 = klsn_map:upsert([Mode, stream_ref], {value, StreamRef}, State0),
    State20 = klsn_map:upsert([Mode, last_websocket_at], {value, Timestamp}, State10),
    {noreply, State20};
handle_info({gun_ws, _Pid, Ref, {text, Text}}, State0) ->
    Timestamp = klsn_flux:timestamp(),
    Mode = mode_from_ref(Ref, State0),
    Hash = klsn_binstr:hash(Text),
    Board = kabue_mufje_types:payload_to_board(jsone:decode(Text)),
    Symbol = maps:get(symbol, Board),
    Exchange = maps:get(exchange, Board),
    Ticker = #{
        symbol => Symbol
      , exchange => Exchange
    },
    State10 = case klsn_map:lookup([Mode, Ticker, current], State0) of
        {value, Previous} ->
            klsn_map:upsert([Mode, data, Ticker, previous], Previous, State0);
        none ->
            State0
    end,
    State20 = klsn_map:upsert([Mode, data, Ticker, current, hash], Hash, State10),
    State30 = klsn_map:upsert([Mode, data, Ticker, current, board], Board, State20),
    State40 = klsn_map:upsert([Mode, data, Ticker, current, timestamp], Timestamp, State30),
    State50 = klsn_map:upsert([Mode, last_updated_at], {value, Timestamp}, State40),
    State60 = klsn_map:upsert([Mode, last_websocket_at], {value, Timestamp}, State50),
    %% ------------------------------------------------------------------
    %% Persist board information to InfluxDB in a separate process to keep
    %% the GenServer loop as lightweight as possible.
    spawn(fun() ->
        Point = board_to_point(Mode, Board, Timestamp),
        {ok, Org} = application:get_env(kabue, influxdb_organization),
        {ok, Bucket} = application:get_env(kabue, influxdb_bucket),
        klsn_flux:write(Org, Bucket, Point)
    end),
    %% ------------------------------------------------------------------
    spawn(fun() ->
        lists:map(fun(SignalPid)->
            SignalPid ! {?MODULE, signal, {update, Mode, Ticker}}
        end, klsn_map:get([Mode, signal, Ticker], State60, []))
    end),
    {noreply, State60};
handle_info(Info, State) ->
    error_logger:info_msg("function=~p:~p/~p, line=~p~ninfo=~p~nstate=~p", [
            ?MODULE
          , ?FUNCTION_NAME
          , ?FUNCTION_ARITY
          , ?LINE
          , Info
          , klsn_map:upsert([real, data], 'OMMIT', State)
        ]),
    {noreply, State}.

terminate(_reason, State) ->
    spawn(fun() ->
        maps:map(fun(_Mode, #{signal:=Signal}) ->
            maps:map(fun(_Ticker, Pids) ->
                lists:map(fun(Pid) ->
                    Pid ! {?MODULE, signal, terminate}
                end, Pids)
            end, Signal)
        end, State)
    end),
    ok.



-spec mode_from_ref(reference(), state()) -> mode().
mode_from_ref(Ref, State) ->
    case State of
        #{ real := #{stream_ref:={value, StreamRef}} } when StreamRef =:= Ref ->
            real;
        #{ test := #{stream_ref:={value, StreamRef}} } when StreamRef =:= Ref ->
            test
    end.

-spec mode_from_pid(pid(), state()) -> mode().
mode_from_pid(Pid, State) ->
    case State of
        #{ real := #{pid:=RealPid} } when RealPid =:= Pid ->
            real;
        #{ test := #{pid:=TestPid} } when TestPid =:= Pid ->
            test
    end.


-spec is_connected(mode()) -> boolean().
is_connected(Mode) ->
    klsn_maybe:get_value(lookup_from_state([Mode, is_upgraded])).

-spec last_updated_at(mode()) -> klsn:maybe(klsn_flux:timestamp()).
last_updated_at(Mode) ->
    klsn_maybe:get_value(lookup_from_state([Mode, last_updated_at])).

-spec last_websocket_at(mode()) -> klsn:maybe(klsn_flux:timestamp()).
last_websocket_at(Mode) ->
    klsn_maybe:get_value(lookup_from_state([Mode, last_websocket_at])).

-spec data_size(mode()) -> non_neg_integer().
data_size(Mode) when Mode =:= real; Mode =:= test ->
    gen_server:call(?MODULE, {data_size, Mode}).


-spec subscribe_signal(mode(), kabue_mufje_rest_apic:ticker()) -> ok.
subscribe_signal(Mode, Ticker) ->
    Pid = self(),
    gen_server:cast(?MODULE, {subscribe_signal, Mode, Ticker, Pid}),
    ok.


-spec unsubscribe_signal(mode(), kabue_mufje_rest_apic:ticker()) -> ok.
unsubscribe_signal(Mode, Ticker) ->
    Pid = self(),
    gen_server:cast(?MODULE, {unsubscribe_signal, Mode, Ticker, Pid}),
    ok.


-spec await_signal(mode(), kabue_mufje_rest_apic:ticker()) -> signal().
await_signal(Mode, Ticker) ->
    receive
        {?MODULE, signal, Signal={update, Mode, Ticker}} ->
            Signal;
        {?MODULE, signal, terminate} ->
            terminate
    end.


-spec flush_signal(signal()) -> ok.
flush_signal(Signal) ->
    receive
        {?MODULE, signal, Signal} ->
            flush_signal(Signal)
    after 0 ->
        ok
    end.


%% ------------------------------------------------------------------
%% InfluxDB helper
%% ------------------------------------------------------------------

-spec board_to_point(
        mode()
      , kabue_mufje_types:board()
      , klsn_flux:timestamp()
    ) -> klsn_flux:point().
board_to_point(Mode, Board, Ts) ->
    {ok, Profile} = application:get_env(kabue, profile),
    {ok, Version} = application:get_env(kabue, version),
    Tag = #{
        profile => Profile,
        version => Version,
        mode => atom_to_binary(Mode, utf8),
        symbol => maps:get(symbol, Board),
        exchange => maps:get(exchange, Board)
    },
    Field = maps:from_list(lists:filtermap(fun({K, V}) ->
        case V of
            undefined -> false;
            null -> false;
            _ ->
                Val1 = case V of
                    A when is_atom(A) -> atom_to_binary(A, utf8);
                    _ -> V
                end,
                {true, {K, Val1}}
        end
    end, maps:to_list(Board))),
    #{
        measurement => mufje_board,
        tag => Tag,
        field => Field,
        timestamp => Ts
    }.


-spec lookup(mode(), kabue_mufje_rest_apic:ticker()) -> klsn:maybe(data()).
lookup(Mode, Ticker) ->
    lookup_from_state([Mode, data, Ticker]).


-spec lookup_from_state(klsn_map:path()) -> klsn:maybe(any()).
lookup_from_state(Path) ->
    gen_server:call(?MODULE, {lookup_from_state, Path}).


