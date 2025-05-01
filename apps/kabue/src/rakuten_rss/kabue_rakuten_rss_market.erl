-module(kabue_rakuten_rss_market).

-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([webhook/1]).

-export([
        lookup/1
      , add/1
      , remove/1
      , last_webhook_at/0
      , last_updated_at/0
      , last_updated_rows/0
      , available_count/0
      , cell/1
      , historical/1
    ]).

-export_type([
        ticker/0
      , column_number/0
      , row_number/0
      , historical_time/0
    ]).

-type ticker() :: klsn:binstr().

-type column_number() :: pos_integer().

-type row_number() :: 2..501.

-type historical_time() :: klsn_flux:timestamp()
                         | klsn_flux:date_time()
                         | #{ s|m|h|d => integer() }
                         | now.

-type state() :: #{
        tickers := maps:map(ticker(), row_number())
      , available_rows := [row_number()]
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
      , last_webhook_at => klsn:maybe(klsn_flux:timestamp())
      , last_updated_at => klsn:maybe(klsn_flux:timestamp())
      , last_updated_cells := [row_number()]
      , on_update := [fun( (state()) -> any() )]
    }.


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Settings) ->
    process_flag(trap_exit, true),
    State = #{
            tickers => #{}
          , available_rows => lists:seq(2, 501)
          , sheet => #{<<"current_etag">> => <<"none">>, <<"data">> => #{}}
          , last_updated_cells => []
          , on_update => [ fun write_updated_jpx_market_info/1 ]
          , update_sheet => maps:from_list(lists:map(fun(I) ->
                {cell(1, I), <<>>}
            end, lists:seq(2, 501)))
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
            {<<"diff">>, Hook#{<<"data">> => Data}}
    end,
    UpdateSheet = klsn_map:get([update_sheet], State0),
    State10 = State0#{
            update_sheet => #{}
          , sheet => Sheet
          , last_updated_cells => klsn_map:get([<<"data">>], Hook)
          , last_webhook_at => klsn_flux:timestamp()
        },
    State30 = case maps:size(klsn_map:get([<<"data">>], Hook)) of
        0 ->
            State10;
        _ ->
            %% Execute on_update callbacks within the same process.  They are
            %% lightweight (stateless writes to InfluxDB) and running them
            %% sequentially avoids spawning an unbounded number of short-lived
            %% processes.
            lists:foreach(fun(OnUpdate) -> OnUpdate(State10) end,
                          maps:get(on_update, State10)),
            State10
    end,
    {reply, jsone:encode(UpdateSheet#{<<"A1">> => A1}), State30};
handle_call({lookup_by_ticker, Ticker}, _From, State) ->
    {reply, jpx_market_info(Ticker, State), State};
handle_call(last_webhook_at, _From, State) ->
    {reply, klsn_map:lookup([last_webhook_at], State), State};
handle_call(last_updated_at, _From, State) ->
    {reply, klsn_map:lookup([last_updated_at], State), State};
handle_call(last_updated_rows, _From, State) ->
    UpdatedRows = sets:to_list(sets:del_element(1, maps:fold(fun(Cell, _, Acc) ->
        Row = cell_name_to_row_number(Cell),
        sets:add_element(Row, Acc)
    end, sets:new([{version,2}]), klsn_map:get([last_updated_cells, <<"data">>], State)))),
    {reply, UpdatedRows, State};
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
            State10 = State0#{
                available_rows => Rows
              , tickers => Tickers
            },
            klsn_map:upsert([update_sheet, cell(1, Row)], <<>>, State10)
    end,
    {noreply, State};
handle_cast({set_last_updated_at, Timestamp}, State0) ->
    State = State0#{
        last_updated_at => Timestamp
    },
    {noreply, State}.


handle_info(Info, State) ->
    logger:info("~p:~p/~p: line=~p info=~p", [
        ?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE, Info]),
    {noreply, State}.

terminate(_reason, _State) ->
    ok.

-spec webhook(klsn:binstr()) -> klsn:binstr().
webhook(Body) ->
    gen_server:call(?MODULE, {webhook, Body}).

-spec lookup(ticker()) -> klsn:maybe(kabue_rakuten_rss_market_types:jpx_market_info()).
lookup(Ticker) ->
    gen_server:call(?MODULE, {lookup_by_ticker, Ticker}).

-spec add(ticker()) -> ok.
add(Ticker) ->
    gen_server:cast(?MODULE, {add_ticker, Ticker}).

-spec remove(ticker()) -> ok.
remove(Ticker) ->
    gen_server:cast(?MODULE, {remove_ticker, Ticker}).

-spec last_updated_at() -> klsn:maybe(klsn_flux:timestamp()).
last_updated_at() ->
    gen_server:call(?MODULE, last_updated_at).

-spec last_webhook_at() -> klsn:maybe(klsn_flux:timestamp()).
last_webhook_at() ->
    gen_server:call(?MODULE, last_webhook_at).

-spec last_updated_rows() -> [row_number()].
last_updated_rows() ->
    gen_server:call(?MODULE, last_updated_rows).

-spec available_count() -> klsn:maybe(non_neg_integer()).
available_count() ->
    gen_server:call(?MODULE, available_count).

-spec jpx_market_info(
        ticker() | row_number(), state()
    ) -> klsn:maybe(kabue_rakuten_rss_market_types:jpx_market_info()).
jpx_market_info(Ticker, State) when is_binary(Ticker) ->
    case klsn_map:lookup([tickers, Ticker], State) of
        {value, RowNumber} ->
            jpx_market_info(RowNumber, State);
        none ->
            none
    end;
jpx_market_info(Row, State) ->
    try
        TypeKeys = kabue_rakuten_rss_market_types:jpx_market_info_types(),
        case
            klsn_map:get([sheet, <<"data">>, cell(2, Row)], State, <<>>)
        of
            <<>> ->
                throw({?MODULE, return, none});
            _ ->
                ok
        end,
        maps:from_list(lists:filtermap(fun({Column, {Type, Key}}) ->
            RawValue = klsn_map:get([sheet, <<"data">>, cell(Column, Row)], State, <<>>),
            case
                kabue_rakuten_rss_market_types:convert(Type, Key, RawValue)
            of
                {value, Value} ->
                    {true, {Key, Value}};
                none ->
                    false
            end
        end, lists:zip(lists:seq(1, length(TypeKeys)), TypeKeys)))
    of
        Value ->
            {value, Value}
    catch
        throw:{?MODULE, return, none} ->
            none
    end.


-spec parse_jpx_market_info(
        non_neg_integer()
      , row_number()
      , klsn:binstr() | maps:map(klsn:binstr(), klsn:binstr())
    ) -> {klsn_flux:key(), klsn:binstr()}.
parse_jpx_market_info(Column, Row, Data) ->
    Keys = kabue_rakuten_rss_market_types:jpx_market_info_keys(),
    parse_jpx_market_info(Column, Row, Data, lists:nth(Column, Keys)).


-spec parse_jpx_market_info(
        non_neg_integer()
      , row_number()
      , klsn:binstr() | maps:map(klsn:binstr(), klsn:binstr())
      , klsn_flux:key()
    ) -> {klsn_flux:key(), klsn:binstr()}.
parse_jpx_market_info(Column, Row, Data, Key) when is_map(Data) ->
    parse_jpx_market_info(
        Column
      , Row 
      , klsn_map:get(cell(Column, Row), Data)
      , Key);
parse_jpx_market_info(_Column, _Row, Value, Key) ->
    {Key, Value}.


-spec cell(non_neg_integer(), row_number()) -> klsn:binstr().
cell(Column, Row) ->
    %% Convert a positive integer column number (1-based) into the
    %% corresponding Excel-style column name (A, B, …, Z, AA, AB, …).
    %%
    %% The previous implementation appended the next letter to the tail of
    %% the accumulator which resulted in the column name being reversed for
    %% numbers larger than 26 (e.g. 28 → "BA").  We now build a list of
    %% letters and finally reverse it once, ensuring correct ordering while
    %% keeping the algorithm tail-recursive.
    ConvertCol = fun Convert(0, Acc) -> Acc;
                       Convert(Col, Acc) ->
                           Col0 = Col - 1,
                           Remainder = Col0 rem 26,
                           Letter = $A + Remainder,
                           NewCol = Col0 div 26,
                           Convert(NewCol, [Letter | Acc])
                end,
    LettersReversed = ConvertCol(Column, []),
    ColumnName = list_to_binary(lists:reverse(LettersReversed)),
    iolist_to_binary([ColumnName, integer_to_binary(Row)]).


-spec cell({non_neg_integer(), row_number()}) -> klsn:binstr();
          (klsn:binstr()) -> {non_neg_integer(), row_number()}.
cell({Column, Row}) ->
    cell(Column, Row);
cell(Cell) ->
    {
        cell_name_to_column_number(Cell)
      , cell_name_to_row_number(Cell)
    }.


-spec cell_name_to_row_number(klsn:binstr()) -> row_number().
cell_name_to_row_number(<<N, _/binary>>=Binary) when $0 =< N, N =< $9 ->
    binary_to_integer(Binary);
cell_name_to_row_number(<<_N, Tail/binary>>) ->
    cell_name_to_row_number(Tail).


cell_name_to_column_number(Binary) ->
    cell_name_to_column_number(Binary, 0).

cell_name_to_column_number(<<A, Rest/binary>>, Acc) when $A =< A, A =< $Z ->
    NewAcc = Acc * 26 + (A - $A + 1),
    cell_name_to_column_number(Rest, NewAcc);
cell_name_to_column_number(_Rest, Acc) ->
    Acc.

-spec write_updated_jpx_market_info(state()) -> ok.
write_updated_jpx_market_info(State) ->
    {ok, Profile} = application:get_env(kabue, profile),
    {ok, Version} = application:get_env(kabue, version),
    Data = maps:fold(fun(Cell, Value, Acc)->
        {Column, Row} = cell(Cell),
        {FieldKey, FieldValue} = parse_jpx_market_info(Column, Row, Value),
        klsn_map:upsert([Row, FieldKey], FieldValue, Acc)
    end, #{}, maps:get(last_updated_cells, State)),
    Points = lists:filtermap(fun({Row, Field}) ->
        case
            {
                klsn_map:lookup([sheet, <<"data">>, cell(1, Row)], State)
              , klsn_map:lookup([sheet, <<"data">>, cell(2, Row)], State)
            }
        of
            {{value, Ticker}, {value, Ticker}} when size(Ticker) > 0 ->
                {true, #{
                    measurement => rakuten_rss_jpx_market_info
                  , tag => #{
                        profile => Profile
                      , version => Version
                      , ticker => Ticker
                    }
                  , field => Field
                }};
            _ ->
                false
        end
    end, maps:to_list(Data)),
    case length(Points) of
        0 -> ok;
        _ ->
            gen_server:cast(
                ?MODULE
              , {
                    set_last_updated_at 
                  , maps:get(last_webhook_at, State)
                }
            )
    end,
    write(Points).


-spec write(klsn_flux:point() | [klsn_flux:point()]) -> ok.
write(Point) ->
    spawn(fun() ->
        {ok, Org} = application:get_env(kabue, influxdb_organization),
        {ok, Bucket} = application:get_env(kabue, influxdb_bucket),
        klsn_flux:write(Org, Bucket, Point)
    end),
    ok.

-spec historical(#{
        ticker => ticker
      , start => historical_time()
      , stop => historical_time()
    }) -> [kabue_rakuten_rss_market_types:jpx_market_info()].
historical(Opts) ->
    {ok, Org} = application:get_env(kabue, influxdb_organization),
    {ok, Bucket} = application:get_env(kabue, influxdb_bucket),
    Ticker = maps:get(ticker, Opts, all),
    Start = historical_time(maps:get(start, Opts, #{d=>7})),
    Stop = historical_time(maps:get(stop, Opts, now)),
    TickerFilter = case Ticker of
        all -> <<>>;
        <<"all">> -> <<>>;
        Enum when Enum =:= list orelse Enum =:= <<"list">> ->
            <<"|> filter(fn: (r) => r[\"_field\"] == \"ticker_name\")
               |> distinct(column: \"_value\")
               |> yield(name: \"ticker_names\")">>;
        _ ->
            <<"|> filter(fn: (r) => r[\"ticker\"] == args.ticker)">>
    end,
    Query = <<"
    from(bucket: args.bucket)
    |> range(start: args.timeRangeStart, stop: args.timeRangeStop)
    |> filter(fn: (r) => r[\"_measurement\"] == args.measurement)
    ", TickerFilter/binary>>,
    Args = #{
        bucket => {string, Bucket}
      , timeRangeStart => Start
      , timeRangeStop => Stop
      , measurement => {string, rakuten_rss_jpx_market_info}
      , ticker => {string, Ticker}
    },
    List = lists:filtermap(fun
        (#{<<"_time">>:=_})-> true;
        (#{<<"result">>:=<<"ticker_names">>}=Doc)->
            {true, #{
                <<"_time">> => klsn_db:time_now()
              , <<"_field">> => maps:get(<<"_field">>, Doc)
              , <<"_value">> => maps:get(<<"_value">>, Doc)
              , <<"ticker">> => maps:get(<<"ticker">>, Doc)
            }};
        (_) -> false
    end, klsn_flux:q(Org, Query, Args)),
    TableMap = lists:foldl(fun(Elem0, Acc)->
        #{<<"_field">>:=Field, <<"_value">>:=Value} = Elem0,
        Elem10 = maps:remove(<<"_field">>, Elem0),
        Elem20 = maps:remove(<<"_value">>, Elem10),
        Elem30 = maps:remove(<<"_start">>, Elem20),
        Elem40 = maps:remove(<<"_stop">>, Elem30),
        Elem50 = maps:remove(<<"table">>, Elem40),
        klsn_map:upsert([Elem50, Field], Value, Acc)
    end, #{}, List),
    {_, TableList} = lists:unzip(lists:sort(lists:map(fun({K, V})->
        {maps:get(<<"_time">>, K), maps:merge(K, V)}
    end, maps:to_list(TableMap)))),
    TypeKeys = kabue_rakuten_rss_market_types:jpx_market_info_types(),
    lists:map(fun(Data)->
        Point = maps:from_list(lists:filtermap(fun({Type, Name})->
            BinName = atom_to_binary(Name),
            case Data of
                #{BinName := BinValue} ->
                    {true, {Name,
                            kabue_rakuten_rss_market_types:convert(Type, Name, BinValue)}};
                _ ->
                    false
            end
        end, TypeKeys)),
        Point#{
            timestamp => calendar:rfc3339_to_system_time(
                binary_to_list(maps:get(<<"_time">>, Data))
              , [{unit, nanosecond}])
        }
    end, TableList).

-spec historical_time(historical_time()) -> klsn_flux:value().
historical_time(now) ->
    {call, now};
historical_time(Timestamp) when is_integer(Timestamp) ->
    klsn_flux:value({timestamp, Timestamp});
historical_time(Time) when is_binary(Time) ->
    klsn_flux:value({date_time, Time});
historical_time(Map) when is_map(Map) ->
    Duration = maps:fold(fun(Unit, Mag, Acc)->
        [{Mag, Unit}|Acc]
    end, [], Map),
    {unary, <<"-">>, {duration, Duration}}.

