-module(kabue_rakuten_rss_market).

-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([webhook/1]).

-export([
        lookup/1
      , add/1
      , remove/1
      , last_updated_at/0
      , available_count/0
    ]).

-export_type([
        ticker/0
    ]).

-type ticker() :: klsn:binstr().

-type state() :: #{
        tickers := maps:map(ticker(), integer())
      , available_rows := [2..500]
      , sheet := #{
              % <<"base_etag">> => <<"-22838601">>
              % <<"current_etag">> => <<"-22838601">>
              % <<"data">> => #{
                      % <<"A1">> => <<"diff">>
                      % <<"A2">> => <<"1234">>
              %     }
            }
      , update_sheet := #{
          % <<"A1">> => <<"diff">>
        }
      , last_updated_at => klsn:maybe(non_neg_integer())
    }.


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Settings) ->
    process_flag(trap_exit, true),
    State = #{
            tickers => #{}
          , available_rows => lists:seq(2, 501)
          , sheet => #{<<"current_etag">> => <<"none">>, <<"data">> => #{}}
          , update_sheet => #{}
        },
    {ok, State}.

handle_call({webhook, Body}, _From, State0) ->
    % spawn(fun() -> io:format("webhook: ~ts~n", [Body]) end),
    Hook = jsone:decode(Body),
    ETag = klsn_map:get([sheet, <<"current_etag">>], State0),
    {A1, Sheet} = case Hook of
        #{<<"base_etag">> := BaseETag} when BaseETag =/= ETag ->
            {<<"full">>, #{
                    <<"current_etag">> => <<"none">>
                  , <<"data">> => #{}
                }};
        _ ->
            Data = maps:merge(
                    klsn_map:get([sheet, <<"data">>], State0)
                  , klsn_map:get([<<"data">>], Hook)
                ),
            write(#{
                measurement => rakuten_rss_market_raw_excel
              , tag => #{ version => 1 }
              , field => Data
            }),
            {<<"diff">>, Hook#{<<"data">> => Data}}
    end,
    UpdateSheet = klsn_map:get([update_sheet], State0),
    LastUpdatedAt = os:system_time(),
    State = State0#{
            update_sheet => #{}
          , sheet => Sheet
          , last_updated_at => LastUpdatedAt
        },
    {reply, jsone:encode(UpdateSheet#{<<"A1">> => A1}), State};
handle_call({lookup_by_ticker, Ticker}, _From, State) ->
    {reply, market_info(Ticker, State), State};
handle_call(last_updated_at, _From, State) ->
    {reply, klsn_map:lookup([last_updated_at], State), State};
handle_call(available_count, _From, State) ->
    {reply, length(klsn_map:get([available_rows], State, [])), State};
handle_call(debug_dump_state, _From, State) ->
    {reply, State, State}.

handle_cast({add_ticker, Ticker}, State0) ->
    State = case klsn_map:lookup([tickers, Ticker], State0) of
        {value, _} ->
            State0;
        none ->
            [Row|Rows] = maps:get(available_rows, State0),
            State10 = klsn_map:upsert([tickers, Ticker], Row, State0#{
                available_rows => Rows
            }),
            klsn_map:upsert([update_sheet, cell(1, Row)], Ticker, State10)
    end,
    {noreply, State};
handle_cast({remove_ticker, Ticker}, State0) ->
    State = case klsn_map:lookup([tickers, Ticker], State0) of
        none ->
            State0;
        {value, Row} ->
            Rows = [Row | maps:get(available_rows, State0)],
            Tickers = maps:remove(Ticker, maps:get(tickers, State0)),
            State0#{
                available_rows => Rows
              , tickers => Tickers
            }
    end,
    {noreply, State}.

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

-spec webhook(klsn:binstr()) -> klsn:binstr().
webhook(Body) ->
    gen_server:call(?MODULE, {webhook, Body}).

-spec lookup(ticker()) -> klsn:maybe(kabue_rakuten_rss_market_types:market_info()).
lookup(Ticker) ->
    gen_server:call(?MODULE, {lookup_by_ticker, Ticker}).

-spec add(ticker()) -> ok.
add(Ticker) ->
    gen_server:cast(?MODULE, {add_ticker, Ticker}).

-spec remove(ticker()) -> ok.
remove(Ticker) ->
    gen_server:cast(?MODULE, {remove_ticker, Ticker}).

-spec last_updated_at() -> klsn:maybe(non_neg_integer()).
last_updated_at() ->
    gen_server:call(?MODULE, last_updated_at).

-spec available_count() -> klsn:maybe(non_neg_integer()).
available_count() ->
    gen_server:call(?MODULE, available_count).

%% private functions %%

-spec market_info(
        ticker(), state()
    ) -> klsn:maybe(kabue_rakuten_rss_market_types:market_info()).
market_info(Ticker, State) ->
    case klsn_map:lookup([tickers, Ticker], State) of
        {value, Row} ->
            Keys = kabue_rakuten_rss_market_types:market_info_keys(),
            {value, maps:from_list(lists:map(fun({Column, Key}) ->
                {Key, klsn_map:get([sheet, <<"data">>, cell(Column, Row)], State, <<>>)}
            end, lists:zip(lists:seq(2, length(Keys)+1), Keys)))};
        none ->
            none
    end.

-spec cell(non_neg_integer(), non_neg_integer()) -> klsn:binstr().
cell(Column, Row) ->
    ConvertCol = fun Convert(Col, Acc) ->
        case Col of
            0 -> 
                Acc;
            _ ->
                Col0 = Col - 1,
                Remainder = Col0 rem 26,
                Letter = $A + Remainder,
                NewCol = Col0 div 26,
                Convert(NewCol, <<Acc/binary, Letter>>)
        end     
    end,
    ColumnName = ConvertCol(Column, <<>>),
    iolist_to_binary([ColumnName, integer_to_binary(Row)]).


-spec write(klsn_flux:point() | [klsn_flux:point()]) -> ok.
write(Point) ->
    spawn(fun() ->
        {ok, Org} = application:get_env(kabue, influxdb_organization),
        {ok, Bucket} = application:get_env(kabue, influxdb_bucket),
        klsn_flux:write(Org, Bucket, Point)
    end),
    ok.



















