-module(kabue_mufje_types).

-export_type([
        board/0
      , date_time/0
    ]).

-export([
        board_keys/0
      , board_types/0
      , payload_to_board/1
      , convert/3
    ]).

-type date_time() :: klsn:binstr().

-type board() :: #{
        symbol => klsn:binstr()
      , symbol_name => klsn:binstr()
      , exchange => kabue_mufje_enum:exchange()
      , exchange_name => klsn:binstr()
      , current_price => float()
      , current_price_time => date_time()
      , current_price_change_status => kabue_mufje_enum:price_change_status()
      , current_price_status => kabue_mufje_enum:price_status()
      , calc_price => float()
      , previous_close => float()
      , previous_close_time => date_time()
      , change_previous_close => float()
      , change_previous_close_per => float()
      , opening_price => float()
      , opening_price_time => date_time()
      , high_price => float()
      , high_price_time => date_time()
      , low_price => float()
      , low_price_time => date_time()
      , trading_volume => float()
      , trading_volume_time => date_time()
      , vwap => float()
      , trading_value => float()
      , bid_qty => float()
      , bid_price => float()
      , bid_time => date_time()
      , bid_sign => kabue_mufje_enum:board_sign()
      , market_order_sell_qty => float()
      , sell1_time => date_time()
      , sell1_sign => kabue_mufje_enum:board_sign()
      , sell1_price => float()
      , sell1_qty => float()
      , sell2_price => float()
      , sell2_qty => float()
      , sell3_price => float()
      , sell3_qty => float()
      , sell4_price => float()
      , sell4_qty => float()
      , sell5_price => float()
      , sell5_qty => float()
      , sell6_price => float()
      , sell6_qty => float()
      , sell7_price => float()
      , sell7_qty => float()
      , sell8_price => float()
      , sell8_qty => float()
      , sell9_price => float()
      , sell9_qty => float()
      , sell10_price => float()
      , sell10_qty => float()
      , ask_qty => float()
      , ask_price => float()
      , ask_time => date_time()
      , ask_sign => kabue_mufje_enum:board_sign()
      , market_order_buy_qty => float()
      , buy1_time => date_time()
      , buy1_sign => kabue_mufje_enum:board_sign()
      , buy1_price => float()
      , buy1_qty => float()
      , buy2_price => float()
      , buy2_qty => float()
      , buy3_price => float()
      , buy3_qty => float()
      , buy4_price => float()
      , buy4_qty => float()
      , buy5_price => float()
      , buy5_qty => float()
      , buy6_price => float()
      , buy6_qty => float()
      , buy7_price => float()
      , buy7_qty => float()
      , buy8_price => float()
      , buy8_qty => float()
      , buy9_price => float()
      , buy9_qty => float()
      , buy10_price => float()
      , buy10_qty => float()
      , oversell_qty => float()
      , underbuy_qty => float()
      , total_market_value => float()
      , settlement_price => float()
      , iv => float()
      , gamma => float()
      , theta => float()
      , vega => float()
      , delta => float()
      , security_type => kabue_mufje_enum:instrument_type()
    }.


-spec payload_to_board(kabue_mufje_rest_apic:payload()) -> board().
payload_to_board(Payload0) ->
    Payload10 = klsn_map:filter(#{
        symbol => klsn_map:lookup([<<"Symbol">>], Payload0)
      , symbol_name => klsn_map:lookup([<<"SymbolName">>], Payload0)
      , exchange => klsn_map:lookup([<<"Exchange">>], Payload0)
      , exchange_name => klsn_map:lookup([<<"ExchangeName">>], Payload0)
      , current_price => klsn_map:lookup([<<"CurrentPrice">>], Payload0)
      , current_price_time => klsn_map:lookup([<<"CurrentPriceTime">>], Payload0)
      , current_price_change_status => klsn_map:lookup([<<"CurrentPriceChangeStatus">>], Payload0)
      , current_price_status => klsn_map:lookup([<<"CurrentPriceStatus">>], Payload0)
      , calc_price => klsn_map:lookup([<<"CalcPrice">>], Payload0)
      , previous_close => klsn_map:lookup([<<"PreviousClose">>], Payload0)
      , previous_close_time => klsn_map:lookup([<<"PreviousCloseTime">>], Payload0)
      , change_previous_close => klsn_map:lookup([<<"ChangePreviousClose">>], Payload0)
      , change_previous_close_per => klsn_map:lookup([<<"ChangePreviousClosePer">>], Payload0)
      , opening_price => klsn_map:lookup([<<"OpeningPrice">>], Payload0)
      , opening_price_time => klsn_map:lookup([<<"OpeningPriceTime">>], Payload0)
      , high_price => klsn_map:lookup([<<"HighPrice">>], Payload0)
      , high_price_time => klsn_map:lookup([<<"HighPriceTime">>], Payload0)
      , low_price => klsn_map:lookup([<<"LowPrice">>], Payload0)
      , low_price_time => klsn_map:lookup([<<"LowPriceTime">>], Payload0)
      , trading_volume => klsn_map:lookup([<<"TradingVolume">>], Payload0)
      , trading_volume_time => klsn_map:lookup([<<"TradingVolumeTime">>], Payload0)
      , vwap => klsn_map:lookup([<<"VWAP">>], Payload0)
      , trading_value => klsn_map:lookup([<<"TradingValue">>], Payload0)
      , bid_qty => klsn_map:lookup([<<"BidQty">>], Payload0)
      , bid_price => klsn_map:lookup([<<"BidPrice">>], Payload0)
      , bid_time => klsn_map:lookup([<<"BidTime">>], Payload0)
      , bid_sign => klsn_map:lookup([<<"BidSign">>], Payload0)
      , market_order_sell_qty => klsn_map:lookup([<<"MarketOrderSellQty">>], Payload0)
      , sell1_time => klsn_map:lookup([<<"Sell1">>, <<"Time">>], Payload0)
      , sell1_sign => klsn_map:lookup([<<"Sell1">>, <<"Sign">>], Payload0)
      , sell1_price => klsn_map:lookup([<<"Sell1">>, <<"Price">>], Payload0)
      , sell1_qty => klsn_map:lookup([<<"Sell1">>, <<"Qty">>], Payload0)
      , sell2_price => klsn_map:lookup([<<"Sell2">>, <<"Price">>], Payload0)
      , sell2_qty => klsn_map:lookup([<<"Sell2">>, <<"Qty">>], Payload0)
      , sell3_price => klsn_map:lookup([<<"Sell3">>, <<"Price">>], Payload0)
      , sell3_qty => klsn_map:lookup([<<"Sell3">>, <<"Qty">>], Payload0)
      , sell4_price => klsn_map:lookup([<<"Sell4">>, <<"Price">>], Payload0)
      , sell4_qty => klsn_map:lookup([<<"Sell4">>, <<"Qty">>], Payload0)
      , sell5_price => klsn_map:lookup([<<"Sell5">>, <<"Price">>], Payload0)
      , sell5_qty => klsn_map:lookup([<<"Sell5">>, <<"Qty">>], Payload0)
      , sell6_price => klsn_map:lookup([<<"Sell6">>, <<"Price">>], Payload0)
      , sell6_qty => klsn_map:lookup([<<"Sell6">>, <<"Qty">>], Payload0)
      , sell7_price => klsn_map:lookup([<<"Sell7">>, <<"Price">>], Payload0)
      , sell7_qty => klsn_map:lookup([<<"Sell7">>, <<"Qty">>], Payload0)
      , sell8_price => klsn_map:lookup([<<"Sell8">>, <<"Price">>], Payload0)
      , sell8_qty => klsn_map:lookup([<<"Sell8">>, <<"Qty">>], Payload0)
      , sell9_price => klsn_map:lookup([<<"Sell9">>, <<"Price">>], Payload0)
      , sell9_qty => klsn_map:lookup([<<"Sell9">>, <<"Qty">>], Payload0)
      , sell10_price => klsn_map:lookup([<<"Sell10">>, <<"Price">>], Payload0)
      , sell10_qty => klsn_map:lookup([<<"Sell10">>, <<"Qty">>], Payload0)
      , ask_qty => klsn_map:lookup([<<"AskQty">>], Payload0)
      , ask_price => klsn_map:lookup([<<"AskPrice">>], Payload0)
      , ask_time => klsn_map:lookup([<<"AskTime">>], Payload0)
      , ask_sign => klsn_map:lookup([<<"AskSign">>], Payload0)
      , market_order_buy_qty => klsn_map:lookup([<<"MarketOrderBuyQty">>], Payload0)
      , buy1_time => klsn_map:lookup([<<"Buy1">>, <<"Time">>], Payload0)
      , buy1_sign => klsn_map:lookup([<<"Buy1">>, <<"Sign">>], Payload0)
      , buy1_price => klsn_map:lookup([<<"Buy1">>, <<"Price">>], Payload0)
      , buy1_qty => klsn_map:lookup([<<"Buy1">>, <<"Qty">>], Payload0)
      , buy2_price => klsn_map:lookup([<<"Buy2">>, <<"Price">>], Payload0)
      , buy2_qty => klsn_map:lookup([<<"Buy2">>, <<"Qty">>], Payload0)
      , buy3_price => klsn_map:lookup([<<"Buy3">>, <<"Price">>], Payload0)
      , buy3_qty => klsn_map:lookup([<<"Buy3">>, <<"Qty">>], Payload0)
      , buy4_price => klsn_map:lookup([<<"Buy4">>, <<"Price">>], Payload0)
      , buy4_qty => klsn_map:lookup([<<"Buy4">>, <<"Qty">>], Payload0)
      , buy5_price => klsn_map:lookup([<<"Buy5">>, <<"Price">>], Payload0)
      , buy5_qty => klsn_map:lookup([<<"Buy5">>, <<"Qty">>], Payload0)
      , buy6_price => klsn_map:lookup([<<"Buy6">>, <<"Price">>], Payload0)
      , buy6_qty => klsn_map:lookup([<<"Buy6">>, <<"Qty">>], Payload0)
      , buy7_price => klsn_map:lookup([<<"Buy7">>, <<"Price">>], Payload0)
      , buy7_qty => klsn_map:lookup([<<"Buy7">>, <<"Qty">>], Payload0)
      , buy8_price => klsn_map:lookup([<<"Buy8">>, <<"Price">>], Payload0)
      , buy8_qty => klsn_map:lookup([<<"Buy8">>, <<"Qty">>], Payload0)
      , buy9_price => klsn_map:lookup([<<"Buy9">>, <<"Price">>], Payload0)
      , buy9_qty => klsn_map:lookup([<<"Buy9">>, <<"Qty">>], Payload0)
      , buy10_price => klsn_map:lookup([<<"Buy10">>, <<"Price">>], Payload0)
      , buy10_qty => klsn_map:lookup([<<"Buy10">>, <<"Qty">>], Payload0)
      , oversell_qty => klsn_map:lookup([<<"OverSellQty">>], Payload0)
      , underbuy_qty => klsn_map:lookup([<<"UnderBuyQty">>], Payload0)
      , total_market_value => klsn_map:lookup([<<"TotalMarketValue">>], Payload0)
      , settlement_price => klsn_map:lookup([<<"SettlementPrice">>], Payload0)
      , iv => klsn_map:lookup([<<"IV">>], Payload0)
      , gamma => klsn_map:lookup([<<"Gamma">>], Payload0)
      , theta => klsn_map:lookup([<<"Theta">>], Payload0)
      , vega => klsn_map:lookup([<<"Vega">>], Payload0)
      , delta => klsn_map:lookup([<<"Delta">>], Payload0)
      , security_type => klsn_map:lookup([<<"SecurityType">>], Payload0)
    }),
    TypeMap = maps:from_list(lists:map(fun({K,V})->
        {V,K}
    end, board_types())),
    Payload20 = maps:map(fun(Name, Value)->
        Type = maps:get(Name, TypeMap),
        convert(Type, Name, Value)
    end, Payload10),
    Payload20.





-spec board_keys() -> [atom()].
board_keys() ->
    {ok, Concrete} = dialyzer_utils:get_core_from_beam(code:which(?MODULE)),
    {ok, Types} = dialyzer_utils:get_record_and_type_info(Concrete),
    MarketInfo = maps:get({type,board,0}, Types),
    lists:map(fun({type, _, _, [{atom, _, Key}|_]})->
        Key
    end, element(4, element(3, element(1, MarketInfo)))).


-spec board_types() -> [{string | float | date_time | {kabue_mufje_enum, atom()}, atom()}].
board_types() ->
    {ok, Concrete} = dialyzer_utils:get_core_from_beam(code:which(?MODULE)),
    {ok, Types} = dialyzer_utils:get_record_and_type_info(Concrete),
    MarketInfo = maps:get({type,board,0}, Types),
    lists:map(fun({type, _, _, [{atom, _, Key}|T]})->
        Type = case T of
            [{remote_type,_,[{atom,_,klsn},{atom,_,binstr},[]]}] ->
                string;
            [{type,_,float,[]}] ->
                float;
            [{user_type,_,date_time,[]}] ->
                date_time;
            [{remote_type,_,[{atom,_,kabue_mufje_enum},{atom,_,EType},[]]}] ->
                {kabue_mufje_enum, EType}
        end,
        {Type, Key}
    end, element(4, element(3, element(1, MarketInfo)))).


-spec convert(string, klsn:binstr(), atom()) -> klsn:binstr();
             (int, klsn:binstr(), atom()) -> integer();
             (float, float(), atom()) -> float().
convert(Type, Key, Value) ->
    try
        case {Type, Value} of
            {string, String} when is_binary(String)->
                String;
            {float, Float} when is_float(Float) ->
                Float;
            {date_time, DateTime} when is_binary(DateTime) ->
                DateTime;
            {{kabue_mufje_enum, EType}, Raw} ->
                case
                    klsn_map:lookup([Raw], klsn_map:invert(kabue_mufje_enum:EType()))
                of
                    {value, Enum} ->
                        Enum;
                    none ->
                        error(badarg)
                end;
            _ ->
                error(badarg)
        end
    catch
        error:badarg ->
            erlang:error(kabue_mufje_types_convert_error, [
                             Type, Key, Value
                         ])
    end.

