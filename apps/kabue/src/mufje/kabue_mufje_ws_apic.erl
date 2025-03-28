-module(kabue_mufje_ws_apic).

-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-export([
        
    ]).

-export_type([
        state/0
      , payload/0
    ]).

-type state() :: #{
        real | test := #{
            pid := pid()
          , stream_ref := reference()
          , kabue_mufje_rest_apic:ticker() => #{
                previous | current => #{
                    hash := klsn:binstr()
                  , board := kabue_mufje_types:board()
                }
            }
        }
    }.


-type payload() :: #{
        
    }.


-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init(nil()) -> {ok, state()}.
init(_Options) ->
    process_flag(trap_exit, true),
    {ok, RealPid} = gun:open("localhost", 18080),
    {ok, http} = gun:await_up(RealPid),
    RealStreamRef = gun:ws_upgrade(RealPid, "/kabusapi/websocket", []),
    Real = #{
        pid => RealPid
      , stream_ref => RealStreamRef
    },
    {ok, TestPid} = gun:open("localhost", 18081),
    {ok, http} = gun:await_up(TestPid),
    TestStreamRef = gun:ws_upgrade(TestPid, "/kabusapi/websocket", []),
    Test = #{
        pid => TestPid
      , stream_ref => TestStreamRef
    },
    State = #{
        real => Real
      , test => Test
    },
    {ok, State}.

handle_call(none, _From, State) ->
    {reply, none, State}.

handle_cast(none, State) ->
    {noreply, State}.

handle_info({gun_upgrade, _Pid, _Ref, _, _}, State) ->
    {noreply, State};
handle_info({gun_ws, _Pid, Ref, {text, Text}}, State0) ->
    Mode = case State0 of
        #{ real := #{stream_ref:=StreamRef} } when StreamRef =:= Ref ->
            real;
        #{ test := #{stream_ref:=StreamRef} } when StreamRef =:= Ref ->
            test
    end,
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
            klsn_map:upsert([Mode, Ticker, previous], Previous, State0);
        none ->
            State0
    end,
    State20 = klsn_map:upsert([Mode, Ticker, current, hash], Hash, State10),
    State30 = klsn_map:upsert([Mode, Ticker, current, board], Board, State20),
    io:format("~p~n", [State30]),
    {noreply, State30};
handle_info(Info, State) ->
    error_logger:info_msg("function=~p:~p/~p, line=~p~ninfo=~p~nstate=~p", [
            ?MODULE
          , ?FUNCTION_NAME
          , ?FUNCTION_ARITY
          , ?LINE
          , Info
          , State
        ]),
    {noreply, State}.

terminate(_reason, _State) ->
    ok.

