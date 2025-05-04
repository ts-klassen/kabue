-module(kabue_mufje_rest_apic).

-export_type([
        options/0
      , request/0
      , payload/0
      , response_left/0
      , either/1
      , symbol/0
      , order_id/0
      , ticker/0
      % custom response types (do not directly expose payload())
      , ranking_result/0
      , symbol_info/0
      , exchange_info/0
      , sendorder_future_result/0
      , sendorder_option_result/0
      , cancelorder_result/0
      , wallet_cash_result/0
      , wallet_future_result/0
      , wallet_margin_result/0
      , wallet_option_result/0
      , order_list_result/0
      , order_detail_result/0
      , position_list_result/0
    ]).


-export([
        request/2
      , token/1
      , ranking/2
      , order/2
      , register/2
      , unregister/2
      , unregister_all/1
      , board/2
      , symbol/2
      , exchange/2
      , sendorder_future/2
      , sendorder_option/2
      , cancelorder/2
      , wallet_cash/1
      , wallet_cash/2
      , wallet_future/1
      , wallet_future/2
      , wallet_margin/1
      , wallet_margin/2
      , wallet_option/1
      , wallet_option/2
      , order_list/2
      , order_detail/2
      , position_list/1
    ]).


-type options() :: #{
        uri_map := uri_string:uri_map()
      , mode := real | test
      , password => fun(() -> klsn:binstr())
      , token => klsn:binstr()
      , on_token_auto_update => fun((klsn:binstr()) -> any())
      , retry_left := non_neg_integer()
      , retry_sleep => non_neg_integer() % milliseconds
      , last_left => response_left()
      , print_left_info_msg => true
    }.

-type request() :: #{
        method := get | post | put
      , uri := klsn:binstr()
      , payload => payload()
      , q => #{}
    }.

-type payload() :: #{}.

-type response_left() :: #{
        resp := term()
      , status => 200..599
      , payload => #{}
      % https://kabucom.github.io/kabusapi/ptal/error.html
      , code => integer()
      , too_many_retry => boolean()
    }.

-type either(A) :: {right, A} | {left, response_left()}.

-type symbol() :: klsn:binstr().

-type order_id() :: klsn:binstr().

-type ticker() :: #{
        symbol => symbol()
      , exchange => kabue_mufje_enum:exchange()
    }.

%%--------------------------------------------------------------------
%% Custom response types for each API endpoint.
%% Currently they are defined as opaque maps.  They are intentionally
%% kept distinct from the generic `payload()` type so that callers can
%% use the more specific names in their specs and pattern matches while
%% we incrementally introduce richer structures.
%%--------------------------------------------------------------------

% Variant for 株価情報 (Type 1-4)
-type ranking_price_entry() :: #{
        no := non_neg_integer()
      , trend := klsn:binstr()
      , average_ranking := float()
      , symbol := klsn:binstr()
      , symbol_name := klsn:binstr()
      , current_price := float()
      , change_ratio := float()
      , change_percentage := float()
      , current_price_time := klsn:binstr()
      , trading_volume := float()
      , turnover := float()
      , exchange_name := klsn:binstr()
      , category_name := klsn:binstr()
    }.

% Variant for TICK回数 (Type 5)
-type ranking_tick_entry() :: #{
        no := non_neg_integer()
      , trend := klsn:binstr()
      , average_ranking := float()
      , symbol := klsn:binstr()
      , symbol_name := klsn:binstr()
      , current_price := float()
      , change_ratio := float()
      , change_percentage := float()
      , tick_count := non_neg_integer()
      , up_count := non_neg_integer()
      , down_count := non_neg_integer()
      , trading_volume := float()
      , turnover := float()
      , exchange_name := klsn:binstr()
      , category_name := klsn:binstr()
    }.


% Variant for 売買高急増 (Type 6)
-type ranking_trade_volume_entry() :: #{
        no := non_neg_integer()
      , trend := klsn:binstr()
      , average_ranking := float()
      , symbol := klsn:binstr()
      , symbol_name := klsn:binstr()
      , current_price := float()
      , change_ratio := float()
      , rapid_trade_percentage := float()
      , trading_volume := float()
      , current_price_time := klsn:binstr()
      , change_percentage := float()
      , exchange_name := klsn:binstr()
      , category_name := klsn:binstr()
    }.

% Variant for 売買代金急増 (Type 7)
-type ranking_trade_value_entry() :: #{
        no := non_neg_integer()
      , trend := klsn:binstr()
      , average_ranking := float()
      , symbol := klsn:binstr()
      , symbol_name := klsn:binstr()
      , current_price := float()
      , change_ratio := float()
      , rapid_payment_percentage := float()
      , turnover := float()
      , current_price_time := klsn:binstr()
      , change_percentage := float()
      , exchange_name := klsn:binstr()
      , category_name := klsn:binstr()
    }.

% Variant for 信用情報 (Type 8-13)
-type ranking_margin_entry() :: #{
        no := non_neg_integer()
      , symbol := klsn:binstr()
      , symbol_name := klsn:binstr()
      , sell_rapid_payment_percentage := float()
      , sell_last_week_ratio := float()
      , buy_rapid_payment_percentage := float()
      , buy_last_week_ratio := float()
      , ratio := float()
      , exchange_name := klsn:binstr()
      , category_name := klsn:binstr()
    }.

% Variant for 業種別指数 (Type 14-15)
-type ranking_category_entry() :: #{
        no := non_neg_integer()
      , trend := klsn:binstr()
      , average_ranking := float()
      , category := klsn:binstr()
      , category_name := klsn:binstr()
      , current_price := float()
      , change_ratio := float()
      , current_price_time := klsn:binstr()
      , change_percentage := float()
    }.

-type ranking_entry() ::
        ranking_price_entry()
      | ranking_tick_entry()
      | ranking_trade_volume_entry()
      | ranking_trade_value_entry()
      | ranking_margin_entry()
      | ranking_category_entry().

-type ranking_result() :: #{
        type := kabue_mufje_enum:ranking_type()
      , exchange_division := kabue_mufje_enum:exchange_division()
      , ranking := [ranking_entry()]
    }.

%%--------------------------------------------------------------------
%% Typed structures for individual REST endpoints
%%--------------------------------------------------------------------

%% /symbol – components/schemas/SymbolSuccess
-type symbol_info() :: #{
        %% always present
        symbol := klsn:binstr()
      , symbol_name := klsn:binstr()

        %% often present (stock/future/option depending on product)
      , display_name        => klsn:binstr()
      , exchange            => kabue_mufje_enum:exchange()
      , exchange_name       => klsn:binstr()
      , bis_category        => klsn:binstr()
      , total_market_value  => float()
      , total_stocks        => float()
      , trading_unit        => float()
      , fiscal_year_end_basic => integer()
      , price_range_group     => klsn:binstr()
      , kc_margin_buy          => float()
      , kc_margin_sell         => float()
      , margin_buy             => float()
      , margin_sell            => float()
      , upper_limit            => float()
      , lower_limit            => float()
      , underlyer              => klsn:binstr()
      , deriv_month            => klsn:binstr()
      , trade_start            => integer()
      , trade_end              => integer()
      , strike_price           => float()
      , put_or_call            => integer()
      , clearing_price         => float()
    }.

%% /exchange – components/schemas/ExchangeResponse
-type exchange_info() :: #{
        symbol := klsn:binstr()
      , bid_price := float()
      , ask_price := float()
      , spread := float()
      , change := float()
      , time := klsn:binstr()
    }.

%% Common acknowledgement structure returned by send-order and cancel-order
-type order_ack() :: #{
        result := non_neg_integer()
      , order_id := order_id()
    }.

-type sendorder_future_result()  :: order_ack().
-type sendorder_option_result()  :: order_ack().
-type cancelorder_result()       :: order_ack().

%% /wallet/cash – components/schemas/WalletCashSuccess
-type wallet_cash_result() :: #{
        stock_account_wallet        := float()
      , au_kc_stock_account_wallet  => float()
      , au_jbn_stock_account_wallet => float()
    }.

%% /wallet/future – components/schemas/WalletFutureSuccess
-type wallet_future_result() :: #{
        future_trade_limit       := float()
      , margin_requirement       => float()
      , margin_requirement_sell  => float()
    }.

%% /wallet/margin – components/schemas/WalletMarginSuccess
-type wallet_margin_result() :: #{
        margin_account_wallet           := float()
      , depositkeep_rate               => float()
      , consignment_deposit_rate       => float()
      , cash_of_consignment_deposit_rate => float()
    }.

%% /wallet/option – components/schemas/WalletOptionSuccess
-type wallet_option_result() :: #{
        option_buy_trade_limit  := float()
      , option_sell_trade_limit := float()
      , margin_requirement      => float()
    }.

%%--------------------------------------------------------------------
%% Orders and positions
%%--------------------------------------------------------------------

%% Detail element inside the "Details" array (execution lines).  Typed very
%% loosely for now—users typically treat it as opaque.
-type order_execution_detail() :: map().

%% Single order entry (components/schemas/OrdersSuccess)
-type order_entry() :: #{
        id := order_id()
      , state := kabue_mufje_enum:order_status()
      , order_state := kabue_mufje_enum:order_status()
      , ord_type := term()
      , recv_time := klsn:binstr()
      , symbol := symbol()
      , symbol_name := klsn:binstr()
      , exchange := kabue_mufje_enum:exchange()
      , exchange_name := klsn:binstr()
      , time_in_force := term()
      , price := float()
      , order_qty := integer()
      , cum_qty := integer()
      , side := kabue_mufje_enum:side()
      , cash_margin := kabue_mufje_enum:cash_margin()
      , account_type := kabue_mufje_enum:account_type()
      , deliv_type := kabue_mufje_enum:deliv_type()
      , expire_day := integer()
      , margin_trade_type := kabue_mufje_enum:margin_trade_type()
      , margin_premium => float()
      , details := [order_execution_detail()]
    }.

-type order_list_result()   :: [order_entry()].
-type order_detail_result() :: [order_entry()].

%% Single position entry (components/schemas/PositionsSuccess)
-type position_entry() :: #{
        execution_id := klsn:binstr()
      , account_type := kabue_mufje_enum:account_type()
      , symbol := symbol()
      , symbol_name := klsn:binstr()
      , exchange := kabue_mufje_enum:exchange()
      , exchange_name := klsn:binstr()
      , security_type := kabue_mufje_enum:security_type()
      , execution_day := integer()
      , price := float()
      , leaves_qty := integer()
      , hold_qty := integer()
      , side := kabue_mufje_enum:side()
      , expenses := float()
      , commission := float()
      , commission_tax := float()
      , expire_day => integer()
      , margin_trade_type => kabue_mufje_enum:margin_trade_type()
      , current_price := float()
      , valuation := float()
      , profit_loss := float()
      , profit_loss_rate := float()
    }.

-type position_list_result() :: [position_entry()].

%%--------------------------------------------------------------------
%% Wallet helpers
%%--------------------------------------------------------------------

-spec payload_to_wallet_cash(map()) -> wallet_cash_result().
payload_to_wallet_cash(Doc) ->
    klsn_map:filter(#{
        stock_account_wallet         => klsn_map:lookup([<<"StockAccountWallet">>], Doc)
      , au_kc_stock_account_wallet   => klsn_map:lookup([<<"AuKCStockAccountWallet">>], Doc)
      , au_jbn_stock_account_wallet  => klsn_map:lookup([<<"AuJbnStockAccountWallet">>], Doc)
    }).


%% /wallet/future ------------------------------------------------------

-spec payload_to_wallet_future(map()) -> wallet_future_result().
payload_to_wallet_future(Doc) ->
    klsn_map:filter(#{
        future_trade_limit       => klsn_map:lookup([<<"FutureTradeLimit">>], Doc)
      , margin_requirement       => klsn_map:lookup([<<"MarginRequirement">>], Doc)
      , margin_requirement_sell  => klsn_map:lookup([<<"MarginRequirementSell">>], Doc)
    }).


%%--------------------------------------------------------------------
%% Internal helpers (REST-only)
%%--------------------------------------------------------------------

-spec payload_to_order_ack(map()) -> order_ack().
payload_to_order_ack(#{<<"Result">> := Res, <<"OrderId">> := Id}) ->
    #{result => Res, order_id => Id}.



-spec ranking(
        #{
            type => kabue_mufje_enum:ranking_type()
          , exchange_division => kabue_mufje_enum:exchange_division()
        }
      , options()
    ) -> either(ranking_result()).
ranking(ReqPayload0, Options) ->
    ReqQuery= maps:from_list(lists:filtermap(fun
        ({type, Type}) ->
            Enum = kabue_mufje_enum:ranking_type(),
            {true, {<<"Type">>, maps:get(Type, Enum)}};
        ({exchange_division, ExchangeDivision}) ->
            Enum = kabue_mufje_enum:exchange_division(),
            {true, {<<"ExchangeDivision">>, maps:get(ExchangeDivision, Enum)}};
        (_) -> false
    end, maps:to_list(ReqPayload0))),
    Res = request(#{
        uri => <<"/kabusapi/ranking">>
      , method => get
      , q => ReqQuery
    } , Options),
    case Res of
        {right, Doc} ->
            RankingList = lists:map(fun(Elem) ->
                %% Build the entry map.
                klsn_map:filter(#{
                    %% Common keys -------------------------------------------------
                    no                              => klsn_map:lookup([<<"No">>], Elem)
                  , trend                           => klsn_map:lookup([<<"Trend">>], Elem)
                  , average_ranking                 => klsn_map:lookup([<<"AverageRanking">>], Elem)
                  , symbol                          => klsn_map:lookup([<<"Symbol">>], Elem)
                  , symbol_name                     => klsn_map:lookup([<<"SymbolName">>], Elem)
                  , current_price                   => klsn_map:lookup([<<"CurrentPrice">>], Elem)
                  , change_ratio                    => klsn_map:lookup([<<"ChangeRatio">>], Elem)
                  , change_percentage               => klsn_map:lookup([<<"ChangePercentage">>], Elem)
                  , current_price_time              => klsn_map:lookup([<<"CurrentPriceTime">>], Elem)
                  , trading_volume                  => klsn_map:lookup([<<"TradingVolume">>], Elem)
                  , turnover                        => klsn_map:lookup([<<"Turnover">>], Elem)
                  , exchange_name                   => klsn_map:lookup([<<"ExchangeName">>], Elem)
                  , category_name                   => klsn_map:lookup([<<"CategoryName">>], Elem)

                    %% TICK count specific ----------------------------------------
                  , tick_count                      => klsn_map:lookup([<<"TickCount">>], Elem)
                  , up_count                        => klsn_map:lookup([<<"UpCount">>], Elem)
                  , down_count                      => klsn_map:lookup([<<"DownCount">>], Elem)

                    %% Volume spike ------------------------------------------------
                  , rapid_trade_percentage          => klsn_map:lookup([<<"RapidTradePercentage">>], Elem)

                    %% Amount spike -------------------------------------------------
                  , rapid_payment_percentage        => klsn_map:lookup([<<"RapidPaymentPercentage">>], Elem)

                    %% Credit information -----------------------------------------
                  , sell_rapid_payment_percentage   => klsn_map:lookup([<<"SellRapidPaymentPercentage">>], Elem)
                  , sell_last_week_ratio            => klsn_map:lookup([<<"SellLastWeekRatio">>], Elem)
                  , buy_rapid_payment_percentage    => klsn_map:lookup([<<"BuyRapidPaymentPercentage">>], Elem)
                  , buy_last_week_ratio             => klsn_map:lookup([<<"BuyLastWeekRatio">>], Elem)
                  , ratio                           => klsn_map:lookup([<<"Ratio">>], Elem)

                    %% Industry index ---------------------------------------------
                  , category                        => klsn_map:lookup([<<"Category">>], Elem)
                })
            end, maps:get(<<"Ranking">>, Doc)),
            RankingResult = #{
                type =>
                    maps:get(
                        maps:get(<<"Type">>, Doc),
                        klsn_map:invert(kabue_mufje_enum:ranking_type())
                    )
              , exchange_division =>
                    maps:get(
                        maps:get(<<"ExchangeDivision">>, Doc),
                        klsn_map:invert(kabue_mufje_enum:exchange_division())
                    )
              , ranking => RankingList
            },
            {right, RankingResult};
        {left, Left} ->
            {left, Left}
    end.





-spec order(
        #{
            symbol := symbol()
          , exchange := kabue_mufje_enum:exchange()
          , security_type := kabue_mufje_enum:security_type()
          , side := kabue_mufje_enum:side()
          , cash_margin := kabue_mufje_enum:cash_margin()
          , margin_trade_type => kabue_mufje_enum:margin_trade_type()
          , margin_premium_unit => float()
          , deliv_type := kabue_mufje_enum:deliv_type()
          , fund_type => kabue_mufje_enum:fund_type()
          , account_type := kabue_mufje_enum:account_type()
          , qty := integer()
          , close_position_order => kabue_mufje_enum:close_position_order()
          , close_positions => [#{
                hold_id := klsn:binstr()
              , qty := integer()
            }]
          , front_order_type := kabue_mufje_enum:front_order_type()
          , price := float
          , expire_day := integer()
          , reverse_limit_order => #{
                trigger_sec := kabue_mufje_enum:trigger_sec()
              , trigger_price := float()
              , under_over := kabue_mufje_enum:under_over()
              , after_hit_order_type := kabue_mufje_enum:after_hit_order_type()
              , after_hit_price := float()
            }
        }
      , options()
    ) -> either(order_id()).
order(ReqPayload0, Options) ->
    ReqPayload = maps:from_list(lists:filtermap(fun
        ({symbol, Symbol}) ->
            {true, {<<"Symbol">>, Symbol}};
        ({exchange, Exchange}) ->
            Enum = kabue_mufje_enum:exchange(),
            {true, {<<"Exchange">>, maps:get(Exchange, Enum)}};
        ({security_type, SecurityType}) ->
            Enum = kabue_mufje_enum:security_type(),
            {true, {<<"SecurityType">>, maps:get(SecurityType, Enum)}};
        ({side, Side}) ->
            Enum = kabue_mufje_enum:side(),
            {true, {<<"Side">>, maps:get(Side, Enum)}};
        ({cash_margin, CashMargin}) ->
            Enum = kabue_mufje_enum:cash_margin(),
            {true, {<<"CashMargin">>, maps:get(CashMargin, Enum)}};
        ({margin_trade_type, MarginTradeType}) ->
            Enum = kabue_mufje_enum:margin_trade_type(),
            {true, {<<"MarginTradeType">>, maps:get(MarginTradeType, Enum)}};
        ({margin_premium_unit, MarginPremiumUnit}) ->
            {true, {<<"MarginPremiumUnit">>, MarginPremiumUnit}};
        ({deliv_type, DelivType}) ->
            Enum = kabue_mufje_enum:deliv_type(),
            {true, {<<"DelivType">>, maps:get(DelivType, Enum)}};
        ({fund_type, FundType}) ->
            Enum = kabue_mufje_enum:fund_type(),
            {true, {<<"FundType">>, maps:get(FundType, Enum)}};
        ({account_type, AccountType}) ->
            Enum = kabue_mufje_enum:account_type(),
            {true, {<<"AccountType">>, maps:get(AccountType, Enum)}};
        ({qty, Qty}) ->
            {true, {<<"Qty">>, Qty}};
        ({close_position_order, ClosePositionOrder}) ->
            Enum = kabue_mufje_enum:close_position_order(),
            Value = maps:get(ClosePositionOrder, Enum),
            {true, {<<"ClosePositionOrder">>, Value}};
        ({close_positions, ClosePositions}) ->
            lists:map(fun(#{hold_id:=HoldID, qty:=Qty})->
                #{<<"HoldID">> => HoldID, <<"Qty">> => Qty}
            end, ClosePositions);
        ({front_order_type, FrontOrderType}) ->
            Enum = kabue_mufje_enum:front_order_type(),
            {true, {<<"FrontOrderType">>,  maps:get(FrontOrderType, Enum)}};
        ({price, Price}) ->
            {true, {<<"Price">>, Price}};
        ({expire_day, ExpireDay}) ->
            {true, {<<"ExpireDay">>, ExpireDay}};
        ({reverse_limit_order, ReverseLimitOrder}) ->
            #{
                <<"TriggerSec">> =>
                    maps:get(
                        maps:get(trigger_sec, ReverseLimitOrder)
                      , kabue_mufje_enum:trigger_sec()
                    )
              , <<"TriggerPrice">> =>
                    maps:get(trigger_price, ReverseLimitOrder)
              , <<"UnderOver">> =>
                    maps:get(
                        maps:get(under_over, ReverseLimitOrder)
                      , kabue_mufje_enum:under_over()
                    )
              , <<"AfterHitOrderType">> =>
                    maps:get(
                        maps:get(after_hit_order_type, ReverseLimitOrder)
                      , kabue_mufje_enum:after_hit_order_type()
                    )
              , <<"AfterHitPrice">> =>
                    maps:get(after_hit_price, ReverseLimitOrder)
            };
        (_) -> false
    end, maps:to_list(ReqPayload0))),
    Res = request(#{
        uri => <<"/kabusapi/sendorder">>
      , method => post
      , payload => ReqPayload
    }, Options),
    case Res of
        {right, #{<<"OrderId">> := Id, <<"Result">> := 0}} ->
            {right, Id};
        {left, Left} ->
            {left, Left}
    end.


-spec register(
        [#{
            symbol => symbol()
          , exchange => kabue_mufje_enum:exchange()
        }]
      , options()
    ) -> either([ticker()]).
register(ReqPayload0, Options) ->
    Symbols = lists:map(fun(Elem) ->
        #{
            <<"Symbol">> => maps:get(symbol, Elem)
          , <<"Exchange">> => maps:get(maps:get(exchange, Elem), kabue_mufje_enum:exchange())
        }
    end, ReqPayload0),
    parse_regist_list(request(#{
        uri => <<"/kabusapi/register">>
      , method => put
      , payload => #{ <<"Symbols">> => Symbols }
    } , Options)).


-spec unregister(
        [#{
            symbol => symbol()
          , exchange => kabue_mufje_enum:exchange()
        }]
      , options()
    ) -> either(ticker()).
unregister(ReqPayload0, Options) ->
    Symbols = lists:map(fun(Elem) ->
        #{
            <<"Symbol">> => maps:get(symbol, Elem)
          , <<"Exchange">> => maps:get(maps:get(exchange, Elem), kabue_mufje_enum:exchange())
        }
    end, ReqPayload0),
    parse_regist_list(request(#{
        uri => <<"/kabusapi/unregister">>
      , method => put
      , payload => #{ <<"Symbols">> => Symbols }
    } , Options)).


-spec unregister_all(options()) -> either([ticker()]).
unregister_all(Options) ->
    parse_regist_list(request(#{
        uri => <<"/kabusapi/unregister/all">>
      , method => put
    } , Options)).


-spec parse_regist_list(either(payload())) -> either(ticker()).
parse_regist_list(Payload) ->
    case Payload of
        {right, #{<<"RegistList">>:=RegistList}} ->
            Enum = klsn_map:invert(kabue_mufje_enum:exchange()),
            Res = lists:map(fun
                (#{<<"Exchange">>:=Exchange, <<"Symbol">>:=Symbol})->
                    #{
                        symbol => Symbol
                      , exchange => maps:get(Exchange, Enum)
                    }
            end, RegistList),
            {right, Res};
        {left, Left} ->
            {left, Left}
    end.


-spec board(ticker(), options()) -> either(kabue_mufje_types:board()).
board(#{symbol := SymbolBin, exchange := ExchangeAtom}, Options) ->
    ExchangeCode = maps:get(ExchangeAtom, kabue_mufje_enum:exchange()),
    Uri = iolist_to_binary([
        "/kabusapi/board/",
        SymbolBin,
        "@",
        klsn_binstr:from_any(ExchangeCode)
    ]),
    case request(#{uri => Uri, method => get}, Options) of
        {right, Payload} ->
            {right, kabue_mufje_types:payload_to_board(Payload)};
        {left, Left} ->
            {left, Left}
    end.


-spec symbol(ticker(), options()) -> either(symbol_info()).

symbol(#{symbol := SymbolBin, exchange := ExchangeAtom}, Options) ->
    %% Build request URI
    ExchangeCode = maps:get(ExchangeAtom, kabue_mufje_enum:exchange()),
    Uri = iolist_to_binary([
        "/kabusapi/symbol/",
        SymbolBin,
        "@",
        klsn_binstr:from_any(ExchangeCode)
    ]),

    %% Perform HTTP call then convert payload
    case request(#{uri => Uri, method => get}, Options) of
        {right, Doc} ->
            EnumExchange = klsn_map:invert(kabue_mufje_enum:exchange()),

            %% Helper to lift Maybe value after enum conversion
            ExchangeMaybe =
                case klsn_map:lookup([<<"Exchange">>], Doc) of
                    {value, Code} -> klsn_map:lookup([Code], EnumExchange);
                    none -> none
                end,

            SymbolInfo = klsn_map:filter(#{
                %% required
                symbol       => klsn_map:lookup([<<"Symbol">>], Doc)
              , symbol_name  => klsn_map:lookup([<<"SymbolName">>], Doc)

                %% optional direct transfers
              , display_name        => klsn_map:lookup([<<"DisplayName">>], Doc)
              , exchange_name       => klsn_map:lookup([<<"ExchangeName">>], Doc)
              , bis_category        => klsn_map:lookup([<<"BisCategory">>], Doc)
              , total_market_value  => klsn_map:lookup([<<"TotalMarketValue">>], Doc)
              , total_stocks        => klsn_map:lookup([<<"TotalStocks">>], Doc)
              , trading_unit        => klsn_map:lookup([<<"TradingUnit">>], Doc)
              , fiscal_year_end_basic => klsn_map:lookup([<<"FiscalYearEndBasic">>], Doc)
              , price_range_group     => klsn_map:lookup([<<"PriceRangeGroup">>], Doc)
              , kc_margin_buy          => klsn_map:lookup([<<"KCMarginBuy">>], Doc)
              , kc_margin_sell         => klsn_map:lookup([<<"KCMarginSell">>], Doc)
              , margin_buy             => klsn_map:lookup([<<"MarginBuy">>], Doc)
              , margin_sell            => klsn_map:lookup([<<"MarginSell">>], Doc)
              , upper_limit            => klsn_map:lookup([<<"UpperLimit">>], Doc)
              , lower_limit            => klsn_map:lookup([<<"LowerLimit">>], Doc)
              , underlyer              => klsn_map:lookup([<<"Underlyer">>], Doc)
              , deriv_month            => klsn_map:lookup([<<"DerivMonth">>], Doc)
              , trade_start            => klsn_map:lookup([<<"TradeStart">>], Doc)
              , trade_end              => klsn_map:lookup([<<"TradeEnd">>], Doc)
              , strike_price           => klsn_map:lookup([<<"StrikePrice">>], Doc)
              , put_or_call            => klsn_map:lookup([<<"PutOrCall">>], Doc)
              , clearing_price         => klsn_map:lookup([<<"ClearingPrice">>], Doc)

                %% converted field (exchange integer -> atom)
              , exchange              => ExchangeMaybe
            }),
            {right, SymbolInfo};
        {left, Left} ->
            {left, Left}
    end.




-spec exchange(kabue_mufje_enum:symbol(), options()) -> either(exchange_info()).

exchange(SymbolAtom, Options) ->
    %% Build URI from symbolic atom
    SymbolBin = maps:get(SymbolAtom, kabue_mufje_enum:symbol()),
    Uri = iolist_to_binary([<<"/kabusapi/exchange/">>, SymbolBin]),

    case request(#{uri => Uri, method => get}, Options) of
        {right, Doc} ->
            ExchangeInfo = klsn_map:filter(#{
                symbol     => klsn_map:lookup([<<"Symbol">>], Doc)
              , bid_price  => klsn_map:lookup([<<"BidPrice">>], Doc)
              , ask_price  => klsn_map:lookup([<<"AskPrice">>], Doc)
              , spread     => klsn_map:lookup([<<"Spread">>], Doc)
              , change     => klsn_map:lookup([<<"Change">>], Doc)
              , time       => klsn_map:lookup([<<"Time">>], Doc)
            }),
            {right, ExchangeInfo};
        {left, Left} ->
            {left, Left}
    end.


-spec sendorder_future(
        #{
            symbol := symbol()
          , exchange := kabue_mufje_enum:exchange()
          , trade_type := integer()
          , time_in_force := integer()
          , side := kabue_mufje_enum:side()
          , qty := integer()
          , price := integer()
          , expire_day := integer()
          , front_order_type := kabue_mufje_enum:front_order_type()
          , reverse_limit_order => #{
                trigger_sec := kabue_mufje_enum:trigger_sec()
              , trigger_price := float()
              , under_over := kabue_mufje_enum:under_over()
              , after_hit_order_type := kabue_mufje_enum:after_hit_order_type()
              , after_hit_price := float()
            }
        }
      , options()
    ) -> either(sendorder_future_result()).

sendorder_future(ReqPayload0, Options) when is_map(ReqPayload0) ->
    Payload = maps:from_list(lists:filtermap(fun
        ({symbol, Symbol}) -> {true, {<<"Symbol">>, Symbol}};
        ({exchange, Exchange}) -> {true, {<<"Exchange">>, maps:get(Exchange, kabue_mufje_enum:exchange())}};
        ({trade_type, Val}) -> {true, {<<"TradeType">>, Val}};
        ({time_in_force, Val}) -> {true, {<<"TimeInForce">>, Val}};
        ({side, Side}) -> {true, {<<"Side">>, maps:get(Side, kabue_mufje_enum:side())}};
        ({qty, Qty}) -> {true, {<<"Qty">>, Qty}};
        ({price, Price}) -> {true, {<<"Price">>, Price}};
        ({expire_day, Day}) -> {true, {<<"ExpireDay">>, Day}};
        ({front_order_type, Type}) -> {true, {<<"FrontOrderType">>, maps:get(Type, kabue_mufje_enum:front_order_type())}};
        ({reverse_limit_order, RLO}) ->
            Map = #{
                <<"TriggerSec">> => maps:get(maps:get(trigger_sec, RLO), kabue_mufje_enum:trigger_sec())
              , <<"TriggerPrice">> => maps:get(trigger_price, RLO)
              , <<"UnderOver">> => maps:get(maps:get(under_over, RLO), kabue_mufje_enum:under_over())
              , <<"AfterHitOrderType">> => maps:get(maps:get(after_hit_order_type, RLO), kabue_mufje_enum:after_hit_order_type())
              , <<"AfterHitPrice">> => maps:get(after_hit_price, RLO)
            },
            {true, {<<"ReverseLimitOrder">>, Map}};
        (_) -> false
    end, maps:to_list(ReqPayload0))),
    case request(#{
            uri => <<"/kabusapi/sendorder/future">>
          , method => post
          , payload => Payload
        }, Options) of
        {right, Doc} ->
            {right, payload_to_order_ack(Doc)};
        {left, Left} ->
            {left, Left}
    end.


-spec sendorder_option(
        #{
            symbol := symbol()
          , exchange := kabue_mufje_enum:exchange()
          , trade_type := integer()
          , time_in_force := integer()
          , side := kabue_mufje_enum:side()
          , qty := integer()
          , price := integer()
          , expire_day := integer()
          , front_order_type := kabue_mufje_enum:front_order_type()
          , reverse_limit_order => #{
                trigger_sec := kabue_mufje_enum:trigger_sec()
              , trigger_price := float()
              , under_over := kabue_mufje_enum:under_over()
              , after_hit_order_type := kabue_mufje_enum:after_hit_order_type()
              , after_hit_price := float()
            }
        }
      , options()
    ) -> either(sendorder_option_result()).
sendorder_option(ReqPayload0, Options) when is_map(ReqPayload0) ->
    Payload = maps:from_list(lists:filtermap(fun
        ({symbol, Symbol}) -> {true, {<<"Symbol">>, Symbol}};
        ({exchange, Exchange}) -> {true, {<<"Exchange">>, maps:get(Exchange, kabue_mufje_enum:exchange())}};
        ({trade_type, Val}) -> {true, {<<"TradeType">>, Val}};
        ({time_in_force, Val}) -> {true, {<<"TimeInForce">>, Val}};
        ({side, Side}) -> {true, {<<"Side">>, maps:get(Side, kabue_mufje_enum:side())}};
        ({qty, Qty}) -> {true, {<<"Qty">>, Qty}};
        ({price, Price}) -> {true, {<<"Price">>, Price}};
        ({expire_day, Day}) -> {true, {<<"ExpireDay">>, Day}};
        ({front_order_type, Type}) -> {true, {<<"FrontOrderType">>, maps:get(Type, kabue_mufje_enum:front_order_type())}};
        ({reverse_limit_order, RLO}) ->
            Map = #{
                <<"TriggerSec">> => maps:get(maps:get(trigger_sec, RLO), kabue_mufje_enum:trigger_sec())
              , <<"TriggerPrice">> => maps:get(trigger_price, RLO)
              , <<"UnderOver">> => maps:get(maps:get(under_over, RLO), kabue_mufje_enum:under_over())
              , <<"AfterHitOrderType">> => maps:get(maps:get(after_hit_order_type, RLO), kabue_mufje_enum:after_hit_order_type())
              , <<"AfterHitPrice">> => maps:get(after_hit_price, RLO)
            },
            {true, {<<"ReverseLimitOrder">>, Map}};
        (_) -> false
    end, maps:to_list(ReqPayload0))),
    case request(#{
            uri => <<"/kabusapi/sendorder/option">>
          , method => post
          , payload => Payload
        }, Options) of
        {right, Doc} -> {right, payload_to_order_ack(Doc)};
        {left, Left} -> {left, Left}
    end.


-spec cancelorder(
        #{
            order_id := order_id()
        }
      , options()) -> either(cancelorder_result()).
cancelorder(#{order_id := OrderId}, Options) ->
    Payload = #{ <<"OrderId">> => OrderId },
    case request(#{
            uri => <<"/kabusapi/cancelorder">>
          , method => put
          , payload => Payload
        }, Options) of
        {right, Doc} -> {right, payload_to_order_ack(Doc)};
        {left, Left} -> {left, Left}
    end.


-spec wallet_cash(options()) -> either(wallet_cash_result()).

wallet_cash(Options) ->
    case request(#{uri => <<"/kabusapi/wallet/cash">>, method => get}, Options) of
        {right, Doc} -> {right, payload_to_wallet_cash(Doc)};
        {left, Left} -> {left, Left}
    end.


-spec wallet_cash(ticker(), options()) -> either(wallet_cash_result()).
wallet_cash(#{symbol := SymbolBin, exchange := ExchangeAtom}, Options) ->
    ExchangeCode = maps:get(ExchangeAtom, kabue_mufje_enum:exchange()),
    Path = iolist_to_binary([
        "/kabusapi/wallet/cash/",
        SymbolBin,
        "@",
        klsn_binstr:from_any(ExchangeCode)
    ]),
    case request(#{uri => Path, method => get}, Options) of
        {right, Doc} -> {right, payload_to_wallet_cash(Doc)};
        {left, Left} -> {left, Left}
    end.


-spec wallet_future(options()) -> either(wallet_future_result()).

wallet_future(Options) ->
    case request(#{uri => <<"/kabusapi/wallet/future">>, method => get}, Options) of
        {right, Doc} -> {right, payload_to_wallet_future(Doc)};
        {left, Left} -> {left, Left}
    end.


-spec wallet_future(ticker(), options()) -> either(wallet_future_result()).
wallet_future(#{symbol := SymbolBin, exchange := ExchangeAtom}, Options) ->
    ExchangeCode = maps:get(ExchangeAtom, kabue_mufje_enum:exchange()),
    Path = iolist_to_binary([
        "/kabusapi/wallet/future/",
        SymbolBin,
        "@",
        klsn_binstr:from_any(ExchangeCode)
    ]),
    case request(#{uri => Path, method => get}, Options) of
        {right, Doc} -> {right, payload_to_wallet_future(Doc)};
        {left, Left} -> {left, Left}
    end.


-spec wallet_margin(options()) -> either(wallet_margin_result()).
%% TODO: (codex)
%% 1. Implement payload_to_wallet_margin/1 locally.
%% 2. Refactor both wallet_margin clauses to use it; drop duplicate TODO.
wallet_margin(Options) ->
    request(#{uri => <<"/kabusapi/wallet/margin">>, method => get}, Options).


-spec wallet_margin(ticker(), options()) -> either(wallet_margin_result()).
wallet_margin(#{symbol := SymbolBin, exchange := ExchangeAtom}, Options) ->
    ExchangeCode = maps:get(ExchangeAtom, kabue_mufje_enum:exchange()),
    Path = iolist_to_binary([
        "/kabusapi/wallet/margin/",
        SymbolBin,
        "@",
        klsn_binstr:from_any(ExchangeCode)
    ]),
    request(#{uri => Path, method => get}, Options).


-spec wallet_option(options()) -> either(wallet_option_result()).
%% TODO: (codex)
%% 1. Implement payload_to_wallet_option/1 here.
%% 2. Refactor both wallet_option clauses to leverage it; remove duplicate TODO.
wallet_option(Options) ->
    request(#{uri => <<"/kabusapi/wallet/option">>, method => get}, Options).


-spec wallet_option(ticker(), options()) -> either(wallet_option_result()).
wallet_option(#{symbol := SymbolBin, exchange := ExchangeAtom}, Options) ->
    ExchangeCode = maps:get(ExchangeAtom, kabue_mufje_enum:exchange()),
    Path = iolist_to_binary([
        "/kabusapi/wallet/option/",
        SymbolBin,
        "@",
        klsn_binstr:from_any(ExchangeCode)
    ]),
    request(#{uri => Path, method => get}, Options).





-spec order_list(
        #{ product => kabue_mufje_enum:product() }
      , options()) -> either(order_list_result()).
%% TODO: (codex) Inline conversion to order_list_result() here.
order_list(Query0, Options) when is_map(Query0) ->
    Q = maps:from_list(lists:filtermap(fun
        ({product, Product}) ->
            {true, {<<"product">>, maps:get(Product, kabue_mufje_enum:product())}};
        (_) -> false
    end, maps:to_list(Query0))),
    request(#{uri => <<"/kabusapi/orders">>, method => get, q => Q}, Options).


-spec order_detail(order_id(), options()) -> either(order_detail_result()).
%% TODO: (codex) Inline conversion to order_detail_result() (single use).
order_detail(OrderIdBin, Options) ->
    Q = #{ <<"id">> => klsn_binstr:from_any(OrderIdBin) },
    request(#{uri => <<"/kabusapi/orders">>, method => get, q => Q}, Options).


-spec position_list(options()) -> either(position_list_result()).
%% TODO: (codex) Inline conversion to position_list_result() within this
%% function.
position_list(Options) ->
    request(#{uri => <<"/kabusapi/positions">>, method => get}, Options).


-spec token(options()) -> either(klsn:binstr()).
token(Options0) ->
    Options = options(Options0),
    Password = case Options of
        #{ password := PasswdFun } ->
            PasswdFun();
        _ ->
            Env = case Options of
                #{mode := real} ->
                    "KABUE_KABUCOM_PASSWORD";
                #{mode := test} ->
                    "KABUE_KABUCOM_TEST_PASSWORD"
            end,
            case os:getenv(Env) of
                false ->
                    <<>>;
                Str when is_list(Str) ->
                    iolist_to_binary(Str)
            end
    end,
    Res = request(#{
        uri => <<"/kabusapi/token">>
      , method => post
      , payload => #{ <<"APIPassword">> => Password }
    }, Options),
    case Res of
        {right, #{<<"ResultCode">> := 0, <<"Token">> := Token}} ->
            {right, Token};
        {left, Left} ->
            {left, Left}
    end.


-spec options() -> options().
options() ->
    #{
      % uri_map => #{
      %     scheme => <<"http">>
      %   , host => <<"localhost">>
      %   , port => 18080
      % }
        mode => test
      , retry_left => 3
      , retry_sleep => 0
    }.


-spec options(options()) -> options().
options(Options0) ->
    Options = maps:merge(options(), Options0),
    case Options of
        #{uri_map := _} ->
            Options;
        #{mode := real} ->
            Options#{
                uri_map => #{
                    scheme => <<"http">>
                  , host => <<"localhost">>
                  , port => 18080
                }
            };
        #{mode := test} ->
            Options#{
                uri_map => #{
                    scheme => <<"http">>
                  , host => <<"localhost">>
                  , port => 18081
                }
            }
    end.


-spec request(request(), options()) -> either(payload()).
request(Request, Options0) ->
    Options = options(Options0),
    try request_(Request, Options) of
        {right, Right} ->
            {right, Right};
        {left, Left=#{status:=401, payload:=#{<<"Code">>:=4001009}}} ->
            RetryOptions = Options#{
                last_left => Left
              , retry_left => maps:get(retry_left, Options) - 1
            },
            case token(RetryOptions) of
                {right, Token} ->
                    case RetryOptions of
                        #{on_token_auto_update:=UpdateFun} ->
                            spawn(fun()->
                                UpdateFun(Token)
                            end);
                        _ ->
                            ok
                    end,
                    request(Request, RetryOptions#{
                        token => Token
                    });
                Other ->
                    Other
            end;
        Other ->
            Other
    catch
        throw:{?MODULE, too_many_retry, Return} ->
            Return
    end.


-spec request_(request(), options()) -> either(payload()).
request_(Request=#{method:=Method, uri:=Uri}, Options) ->
    case Options of
        #{retry_left := N, retry_sleep := Sleep} when N > 0 ->
            timer:sleep(Sleep);
        #{last_left := LastLeft} ->
            throw({?MODULE, too_many_retry, {left, LastLeft#{
                too_many_retry => true
            }}});
        _ ->
            error(too_many_retry)
    end,
    UriMap10 = klsn_map:get([uri_map], Options),
    UriMap20 = UriMap10#{ path => Uri },
    UriMap30 = case Request of
        #{ q := Query } ->
            QueryStr = uri_string:compose_query(lists:map(fun
                ({Key, Val}) ->
                    {Key, klsn_binstr:from_any(Val)};
                (Pair) -> Pair
            end, maps:to_list(Query))),
            UriMap20#{'query' => QueryStr};
        _ ->
            UriMap20
    end,
    Url = uri_string:recompose(UriMap30),
    ReqHeaders = case Options of
        #{token := Token} ->
            [{"X-API-KEY", Token}];
        _ ->
            []
    end,
    Req = case klsn_map:lookup([payload], Request) of
        none ->
            {Url, ReqHeaders};
        {value, JSON} when is_map(JSON) ->
            {Url, ReqHeaders, "application/json", jsone:encode(JSON)}
    end,
    HttpcRes = httpc:request(Method, Req, [], [{body_format, binary}]),
    case HttpcRes of
        {ok, {{_, Status, _}, _ResHeaders, ResBody}} when 200 =< Status, Status =< 299 ->
            {right, jsone:decode(ResBody)};
        {ok, {{_, Status, _}, _ResHeaders, ResBody}} ->
            Left0 = #{
                resp => HttpcRes
              , status => Status
            },
            Left10 = try jsone:decode(ResBody) of
                LeftPayload ->
                    Left0#{
                        payload => LeftPayload
                    }
            catch
                error:badarg ->
                    Left0
            end,
            Left = case klsn_map:lookup([payload, <<"Code">>], Left10) of
                {value, Code} ->
                    Left10#{
                        code => Code
                    };
                none ->
                    Left10
            end,
            case
                {
                    klsn_map:lookup([payload, <<"Code">>], Left10)
                  , klsn_map:lookup([payload, <<"Message">>], Left10)
                  , klsn_map:lookup([print_left_info_msg], Options)
                }
            of
                {{value, CodeMsg}, {value, Message}, {value, true}} ->
                    error_logger:info_msg("~p:~p/~p error: ~p~n~ts~n~p~n", [
                        ?MODULE
                      , ?FUNCTION_NAME
                      , ?FUNCTION_ARITY
                      , CodeMsg
                      , Message
                      , Request
                    ]);
                _ ->
                    ok
            end,
            {left, Left};
        _ ->
            {left, #{ resp => HttpcRes }}
    end.

% lists:map(fun(#{<<"Symbol">>:=Ticker})-> kabue_rakuten_rss_market:add(Ticker) end, maps:get(<<"Ranking">>, element(2, kabue_mufje_rest_apic:ranking(trading_volume_top, #{})))).


