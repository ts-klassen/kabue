-module(kabue_rpc).

-export([
        echo/1
      , board/1
      , quick_take/1
      , wallet/1
      , position_list/1
      , order_list/1
      , panic_exit/1
    ]).

echo(Payload) ->
    Payload.

board(#{<<"symbol">>:=Symbol}) ->
    Ticker = #{
        symbol => Symbol
      , exchange => tokyo
    },
    case kabue_mufje_ws_apic:lookup(real, Ticker) of
        {value, Data} ->
            #{
                is_ws_data => true
              , board => klsn_map:get([current, board], Data)
              , time => klsn_map:get([current, timestamp], Data)
            };
        none ->
            {right, Token} = kabue_mufje_rest_apic:token(#{ mode => real}),
            Options = #{ mode => real, print_left_info_msg => true, token => Token },
            kabue_mufje_rest_apic:register([Ticker], Options),
            {right, Board} = kabue_mufje_rest_apic:board(Ticker, Options),
            #{
                is_ws_data => false
              , board => Board
              , time => klsn_flux:timestamp()
            }
    end.


quick_take(#{<<"symbol">>:=Symbol}) ->
    <<"1459">> = Symbol,
    Ticker = #{
        symbol => Symbol
      , exchange => tokyo
    },
    Qty = 1,
    Profit = 1,
    {right, Token} = kabue_mufje_rest_apic:token(#{ mode => real}),
    Options = #{ mode => real, print_left_info_msg => true, token => Token },
    {right, OrderId} = kabue_mufje_rest_apic:order(
        #{
            symbol => maps:get(symbol, Ticker)
          , exchange => maps:get(exchange, Ticker)
          , security_type => stock
          , side => buy
          , cash_margin => margin_new
          , margin_trade_type => general_daytrade
          % margin_premium_unit => float()
          , deliv_type => unspecified
          , fund_type => credit_trading
          , account_type => specific
          , qty => Qty
          % close_position_order => kabue_mufje_enum:close_position_order()
          % close_positions => [#{
          %     hold_id => klsn:binstr()
          %   , qty => integer()
          % }]
          , front_order_type => market
          , price => 0
          , expire_day => 0
          % reverse_limit_order => #{
          %     trigger_sec => kabue_mufje_enum:trigger_sec()
          %   , trigger_price => float()
          %   , under_over => kabue_mufje_enum:under_over()
          %   , after_hit_order_type => kabue_mufje_enum:after_hit_order_type()
          %   , after_hit_price => float()
          % }
        }
      , Options
    ),
    timer:sleep(1000),
    {right, OrderDetail} = kabue_mufje_rest_apic:order_detail(OrderId, Options),
    BuyPriceMax = lists:max(lists:map(fun(#{price:=P}) ->
        P
    end, maps:get(details, OrderDetail))),
    Price = BuyPriceMax + Profit,
    {right, CloseOrderId} = kabue_mufje_rest_apic:order(
        #{
            symbol => maps:get(symbol, Ticker)
          , exchange => maps:get(exchange, Ticker)
          , security_type => stock
          , side => sell
          , cash_margin => margin_repay
          , margin_trade_type => general_daytrade
          % margin_premium_unit => float()
          , deliv_type => deposit
          , fund_type => credit_trading
          , account_type => specific
          , qty => Qty
          , close_position_order => order_old_date_high_profit
          % close_positions => [#{
          %     hold_id => klsn:binstr()
          %   , qty => integer()
          % }]
          , front_order_type => nonlimit_open_afternoon
          , price => Price
          , expire_day => 0
          % reverse_limit_order => #{
          %     trigger_sec => kabue_mufje_enum:trigger_sec()
          %   , trigger_price => float()
          %   , under_over => kabue_mufje_enum:under_over()
          %   , after_hit_order_type => kabue_mufje_enum:after_hit_order_type()
          %   , after_hit_price => float()
          % }
        }
      , Options
    ),
    #{
        order_id => OrderId
      , close_order_id => CloseOrderId
      , close_order_price => Price
    }.


wallet(#{}) ->
    {right, Token} = kabue_mufje_rest_apic:token(#{ mode => real}),
    Options = #{ mode => real, print_left_info_msg => true, token => Token },
    {right, Cash} = kabue_mufje_rest_apic:wallet_cash(Options),
    {right, Margin} = kabue_mufje_rest_apic:wallet_margin(Options),
    #{
        cash => Cash
      , margin => Margin
    }.


position_list(#{}) ->
    {right, Token} = kabue_mufje_rest_apic:token(#{ mode => real}),
    Options = #{ mode => real, print_left_info_msg => true, token => Token },
    {right, PositionList} = kabue_mufje_rest_apic:position_list(Options),
    #{
        position_list => PositionList
    }.


order_list(#{}) ->
    {right, Token} = kabue_mufje_rest_apic:token(#{ mode => real}),
    Options = #{ mode => real, print_left_info_msg => true, token => Token },
    {right, OrderList} = kabue_mufje_rest_apic:order_list(
        #{product => all}
      , Options
    ),
    Ids = lists:foldl(fun
        (#{order_qty:=O, cum_qty:=C, id:=I}, #{all:=L}=Acc) when abs(O-C) < 0.001 ->
            Acc#{all := [I|L]};
        (#{cum_qty:=C, id:=I}, #{none:=L}=Acc) when C < 0.001 ->
            Acc#{none := [I|L]};
        (#{id:=I}, #{partial:=L}=Acc) ->
            Acc#{partial := [I|L]}
    end, #{all => [], partial => [], none => []}, OrderList),
    #{
        order_list => OrderList
      , id_buckets => Ids
    }.


panic_exit(#{}) ->
    {right, Token} = kabue_mufje_rest_apic:token(#{ mode => real}),
    Options = #{ mode => real, print_left_info_msg => true, token => Token },
    {right, OrderList} = kabue_mufje_rest_apic:order_list(
        #{product => all}
      , Options
    ),
    IdsToCancel = lists:filtermap(fun
        (#{state:=finished}) ->
            false;
        (#{state:=_, id:=I}) ->
            % We cancel everything we can cancel. Including margin_repay
            {true, I}
    end, OrderList),
    CancelResult = lists:map(fun(OID) ->
        kabue_mufje_rest_apic:cancel_order(#{order_id => OID}, Options)
    end, IdsToCancel),
    {right, PositionList} = kabue_mufje_rest_apic:position_list(Options),
    ExitList = lists:filtermap(fun
        (#{leaves_qty:=LQty}) when LQty < 0.001 ->
            false;
        (Pos=#{margin_trade_type:=MTT, side:=Side0}) ->
            Side = case Side0 of
                buy -> sell;
                sell -> buy
            end,
            {true, #{
                symbol => maps:get(symbol, Pos)
              , exchange => maps:get(exchange, Pos)
              , security_type => stock
              , side => Side
              , cash_margin => margin_repay
              , margin_trade_type => MTT
              % margin_premium_unit => float()
              , deliv_type => deposit
              , fund_type => credit_trading
              , account_type => specific
              , qty => maps:get(leaves_qty, Pos)
              , close_position_order => order_old_date_high_profit
              % close_positions => [#{
              %     hold_id => klsn:binstr()
              %   , qty => integer()
              % }]
              , front_order_type => market
              , price => 0
              , expire_day => 0
              % reverse_limit_order => #{
              %     trigger_sec => kabue_mufje_enum:trigger_sec()
              %   , trigger_price => float()
              %   , under_over => kabue_mufje_enum:under_over()
              %   , after_hit_order_type => kabue_mufje_enum:after_hit_order_type()
              %   , after_hit_price => float()
              % }
            }};
        (Pos=#{margin_trade_type:=_}) ->
            error({unreachable, Pos});
        (Pos=#{side:=buy}) ->
            {true, #{
                symbol => maps:get(symbol, Pos)
              , exchange => maps:get(exchange, Pos)
              , security_type => stock
              , side => sell
              , cash_margin => spot
              % margin_trade_type => MTT
              % margin_premium_unit => float()
              , deliv_type => unspecified
              , fund_type => cash_sale
              , account_type => specific
              , qty => maps:get(leaves_qty, Pos)
              , close_position_order => order_old_date_high_profit
              % close_positions => [#{
              %     hold_id => klsn:binstr()
              %   , qty => integer()
              % }]
              , front_order_type => market
              , price => 0
              , expire_day => 0
              % reverse_limit_order => #{
              %     trigger_sec => kabue_mufje_enum:trigger_sec()
              %   , trigger_price => float()
              %   , under_over => kabue_mufje_enum:under_over()
              %   , after_hit_order_type => kabue_mufje_enum:after_hit_order_type()
              %   , after_hit_price => float()
              % }
            }};
        (Pos) ->
            error({unreachable, Pos})
    end, PositionList),
    ExitResult = lists:map(fun(Order) ->
        kabue_mufje_rest_apic:order(Order, Options)
    end, ExitList),
    Either2Success = fun
        ({right, Payload}) ->
            #{success => true, payload => Payload};
        ({left, Payload}) ->
            #{success => false, payload => Payload}
    end,
    Success = lists:all(fun
        ({righe, _}) -> true;
        ({left, _}) -> false
    end, CancelResult ++ ExitResult),
    #{
        success => Success
      , cancel_list => IdsToCancel
      , exit_list => ExitList
      , cancel_result => lists:map(Either2Success, CancelResult)
      , exit_result => lists:map(Either2Success, ExitResult)
    }.

