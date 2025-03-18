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
    ]).

-export_type([
        ticker/0
      , column_number/0
      , row_number/0
    ]).

-type ticker() :: klsn:binstr().

-type column_number() :: pos_integer().

-type row_number() :: 2..501.

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
      , last_webhook_at => klsn:maybe(non_neg_integer())
      , last_updated_at => klsn:maybe(non_neg_integer())
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
          , update_sheet => #{}
          , last_updated_cells => []
          , on_update => [ fun write_updated_market_info/1 ]
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
    TimeNow = klsn_flux:timestamp(),
    State10 = State0#{
            update_sheet => #{}
          , sheet => Sheet
          , last_updated_cells => klsn_map:get([<<"data">>], Hook)
          , last_webhook_at => TimeNow
        },
    State30 = case maps:size(klsn_map:get([<<"data">>], Hook)) of
        0 ->
            State10;
        _ ->
            State20 = State10#{
                last_updated_at => TimeNow
            },
            lists:foreach(fun(OnUpdate) ->
                spawn(fun() -> OnUpdate(State20) end)
            end, maps:get(on_update, State20)),
            State20
    end,
    {reply, jsone:encode(UpdateSheet#{<<"A1">> => A1}), State30};
handle_call({lookup_by_ticker, Ticker}, _From, State) ->
    {reply, market_info(Ticker, State), State};
handle_call(last_webhook_at, _From, State) ->
    {reply, klsn_map:lookup([last_webhook_at], State), State};
handle_call(last_updated_at, _From, State) ->
    {reply, klsn_map:lookup([last_updated_at], State), State};
handle_call(last_updated_rows, _From, State) ->
    UpdatedRows = sets:to_list(sets:del_element(1, maps:fold(fun(Cell, _, Acc) ->
        Row = cell_name_to_row_number(Cell),
        sets:add_element(Row, Acc)
    end, sets:new([{version,2}]), klsn_map:get([last_updated_cells, <<"data">>], State)))),
    {reply, klsn_map:get([last_updated_rows], State), State};
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

-spec last_webhook_at() -> klsn:maybe(non_neg_integer()).
last_webhook_at() ->
    gen_server:call(?MODULE, last_webhook_at).

-spec last_updated_rows() -> [row_number()].
last_updated_rows() ->
    gen_server:call(?MODULE, last_updated_rows).

-spec available_count() -> klsn:maybe(non_neg_integer()).
available_count() ->
    gen_server:call(?MODULE, available_count).

%% private functions %%

-spec market_info(
        ticker() | row_number(), state()
    ) -> klsn:maybe(kabue_rakuten_rss_market_types:market_info()).
market_info(Ticker, State) when is_binary(Ticker) ->
    case klsn_map:lookup([tickers, Ticker], State) of
        {value, RowNumber} ->
            market_info(RowNumber, State);
        none ->
            none
    end;
market_info(Row, State) ->
    try
        Keys = kabue_rakuten_rss_market_types:market_info_keys(),
        case
            klsn_map:get([sheet, <<"data">>, cell(2, Row)], State, <<>>)
        of
            <<>> ->
                throw({?MODULE, return, none});
            _ ->
                ok
        end,
        maps:from_list(lists:map(fun({Column, Key}) ->
            {Key, klsn_map:get([sheet, <<"data">>, cell(Column, Row)], State, <<>>)}
        end, lists:zip(lists:seq(1, length(Keys)), Keys)))
    of
        Value ->
            {value, Value}
    catch
        throw:{?MODULE, return, none} ->
            none
    end.


-spec parse_market_info(
        non_neg_integer()
      , row_number()
      , klsn:binstr() | maps:map(klsn:binstr(), klsn:binstr())
    ) -> {klsn_flux:key(), klsn:binstr()}.
parse_market_info(Column, Row, Data) ->
    Keys = kabue_rakuten_rss_market_types:market_info_keys(),
    parse_market_info(Column, Row, Data, lists:nth(Column, Keys)).


-spec parse_market_info(
        non_neg_integer()
      , row_number()
      , klsn:binstr() | maps:map(klsn:binstr(), klsn:binstr())
      , klsn_flux:key()
    ) -> {klsn_flux:key(), klsn:binstr()}.
parse_market_info(Column, Row, Data, Key) when is_map(Data) ->
    parse_market_info(
        Column
      , Row 
      , klsn_map:get(cell(Column, Row), Data)
      , Key);
parse_market_info(_Column, _Row, Value, Key) ->
    {Key, Value}.


-spec cell(non_neg_integer(), row_number()) -> klsn:binstr().
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

-spec write_updated_market_info(state()) -> ok.
write_updated_market_info(State) ->
    {ok, Profile} = application:get_env(kabue, profile),
    {ok, Version} = application:get_env(kabue, version),
    Data = maps:fold(fun(Cell, Value, Acc)->
        {Column, Row} = cell(Cell),
        {FieldKey, FieldValue} = parse_market_info(Column, Row, Value),
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
                    measurement => rakuten_rss_market_info
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
    write(Points).


-spec write(klsn_flux:point() | [klsn_flux:point()]) -> ok.
write(Point) ->
    spawn(fun() ->
        {ok, Org} = application:get_env(kabue, influxdb_organization),
        {ok, Bucket} = application:get_env(kabue, influxdb_bucket),
        klsn_flux:write(Org, Bucket, Point)
    end),
    ok.
















