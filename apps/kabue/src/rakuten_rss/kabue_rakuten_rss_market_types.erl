-module(kabue_rakuten_rss_market_types).

-export_type([
        market_info/0
    ]).

-export([
        market_info_keys/0
    ]).

-type market_info() :: #{
    % 引数として指定した ticker
    ticker := klsn:binstr(),
    % 1 銘柄コード
    ticker_code := klsn:binstr(),
    % 2 市場コード
    market_code := klsn:binstr(),
    % 3 銘柄名称
    ticker_name := klsn:binstr(),
    % 4 市場名称
    market_name := klsn:binstr(),
    % 5 市場部名称
    market_section_name := klsn:binstr(),
    % 6 市場部略称
    market_section_abbr := klsn:binstr(),
    % 7 現在日付 (YYYY/MM/DD)
    current_date := klsn:binstr(),
    % 8 現在値
    current_price := number(),
    % 9 現在値時刻 (HH:MM)
    current_price_time := klsn:binstr(),
    % 10 現在値詳細時刻 (HH:MM:SS)
    current_price_detail_time := klsn:binstr(),
    % 11 現在値ティック
    current_tick := integer(),
    % 12 現在値フラグ
    current_price_flag := klsn:binstr(),
    % 13 前日比
    previous_diff := number(),
    % 14 前日比率
    previous_ratio := number(),
    % 15 前日終値
    previous_close := number(),
    % 16 前日日付 (YYYY/MM/DD)
    previous_date := klsn:binstr(),
    % 17 前日終値フラグ
    previous_close_flag := klsn:binstr(),
    % 18 歩み1
    tick1 := number(),
    % 19 歩み2
    tick2 := number(),
    % 20 歩み3
    tick3 := number(),
    % 21 歩み4
    tick4 := number(),
    % 22 歩み1詳細時刻 (HH:MM:SS)
    tick1_time := klsn:binstr(),
    % 23 歩み2詳細時刻 (HH:MM:SS)
    tick2_time := klsn:binstr(),
    % 24 歩み3詳細時刻 (HH:MM:SS)
    tick3_time := klsn:binstr(),
    % 25 歩み4詳細時刻 (HH:MM:SS)
    tick4_time := klsn:binstr(),
    % 26 出来高
    volume := integer(),
    % 27 売買代金
    trade_value := integer(),
    % 28 出来高加重平均
    volume_weighted_avg := number(),
    % 29 時価総額
    market_cap := number(),
    % 30 始値
    open_price := number(),
    % 31 高値
    high_price := number(),
    % 32 安値
    low_price := number(),
    % 33 始値時刻 (HH:MM)
    open_time := klsn:binstr(),
    % 34 高値時刻 (HH:MM)
    high_time := klsn:binstr(),
    % 35 安値時刻 (HH:MM)
    low_time := klsn:binstr(),
    % 36 始値詳細時刻 (HH:MM:SS)
    open_detail_time := klsn:binstr(),
    % 37 高値詳細時刻 (HH:MM:SS)
    high_detail_time := klsn:binstr(),
    % 38 安値詳細時刻 (HH:MM:SS)
    low_detail_time := klsn:binstr(),
    % 39 前場始値
    morning_open := number(),
    % 40 前場高値
    morning_high := number(),
    % 41 前場安値
    morning_low := number(),
    % 42 前場終値
    morning_close := number(),
    % 43 前場出来高
    morning_volume := integer(),
    % 44 後場始値
    afternoon_open := number(),
    % 45 後場高値
    afternoon_high := number(),
    % 46 後場安値
    afternoon_low := number(),
    % 47 前場始値時刻 (HH:MM)
    morning_open_time := klsn:binstr(),
    % 48 前場高値時刻 (HH:MM)
    morning_high_time := klsn:binstr(),
    % 49 前場安値時刻 (HH:MM)
    morning_low_time := klsn:binstr(),
    % 50 前場終値時刻 (HH:MM)
    morning_close_time := klsn:binstr(),
    % 51 前場出来高時刻 (HH:MM)
    morning_volume_time := klsn:binstr(),
    % 52 後場始値時刻 (HH:MM)
    afternoon_open_time := klsn:binstr(),
    % 53 後場高値時刻 (HH:MM)
    afternoon_high_time := klsn:binstr(),
    % 54 後場安値時刻 (HH:MM)
    afternoon_low_time := klsn:binstr(),
    % 55 最良売気配値
    best_sell_price := number(),
    % 56 最良買気配値
    best_buy_price := number(),
    % 57 最良売気配数量
    best_sell_volume := integer(),
    % 58 最良買気配数量
    best_buy_volume := integer(),
    % 59 最良売気配時刻 (HH:MM)
    best_sell_time := klsn:binstr(),
    % 60 最良買気配時刻 (HH:MM)
    best_buy_time := klsn:binstr(),
    % 61 最良売気配詳細時刻 (HH:MM:SS)
    best_sell_detail_time := klsn:binstr(),
    % 62 最良買気配詳細時刻 (HH:MM:SS)
    best_buy_detail_time := klsn:binstr(),
    % 63 特別売気配フラグ
    special_sell_flag := klsn:binstr(),
    % 64 特別買気配フラグ
    special_buy_flag := klsn:binstr(),
    % 65 信用貸借区分 (1:貸借、2:信用)
    credit_lending_type := integer(),
    % 66 逆日歩
    reverse_interest := number(),
    % 67 逆日歩更新日付 (YYYY/MM/DD)
    reverse_interest_date := klsn:binstr(),
    % 68 信用売残
    credit_sell_balance := integer(),
    % 69 信用売残前週比
    credit_sell_prev_week_diff := number(),
    % 70 信用買残
    credit_buy_balance := integer(),
    % 71 信用買残前週比
    credit_buy_prev_week_diff := number(),
    % 72 信用倍率
    credit_ratio := number(),
    % 73 証金残更新日付 (YYYY/MM/DD)
    margin_balance_update_date := klsn:binstr(),
    % 74 新規貸株
    new_lending_stock := integer(),
    % 75 新規融資
    new_financing := integer(),
    % 76 返済貸株
    repaid_lending_stock := integer(),
    % 77 返済融資
    repaid_financing := integer(),
    % 78 残高貸株
    balance_lending_stock := integer(),
    % 79 残高融資
    balance_financing := integer(),
    % 80 残高差引
    balance_diff := integer(),
    % 81 前日比貸株
    prev_lending_diff := number(),
    % 82 前日比融資
    prev_financing_diff := number(),
    % 83 前日比差引
    net_diff := number(),
    % 84 回転日数
    turnover_days := integer(),
    % 85 貸借倍率
    lending_ratio := number(),
    % 86 最良売気配値1
    best_sell_price_1 := number(),
    % 87 最良売気配値2
    best_sell_price_2 := number(),
    % 88 最良売気配値3
    best_sell_price_3 := number(),
    % 89 最良売気配値4
    best_sell_price_4 := number(),
    % 90 最良売気配値5
    best_sell_price_5 := number(),
    % 91 最良売気配値6
    best_sell_price_6 := number(),
    % 92 最良売気配値7
    best_sell_price_7 := number(),
    % 93 最良売気配値8
    best_sell_price_8 := number(),
    % 94 最良売気配値9
    best_sell_price_9 := number(),
    % 95 最良売気配値10
    best_sell_price_10 := number(),
    % 96 最良買気配値1
    best_buy_price_1 := number(),
    % 97 最良買気配値2
    best_buy_price_2 := number(),
    % 98 最良買気配値3
    best_buy_price_3 := number(),
    % 99 最良買気配値4
    best_buy_price_4 := number(),
    % 100 最良買気配値5
    best_buy_price_5 := number(),
    % 101 最良買気配値6
    best_buy_price_6 := number(),
    % 102 最良買気配値7
    best_buy_price_7 := number(),
    % 103 最良買気配値8
    best_buy_price_8 := number(),
    % 104 最良買気配値9
    best_buy_price_9 := number(),
    % 105 最良買気配値10
    best_buy_price_10 := number(),
    % 106 最良売気配数量1
    best_sell_volume_1 := integer(),
    % 107 最良売気配数量2
    best_sell_volume_2 := integer(),
    % 108 最良売気配数量3
    best_sell_volume_3 := integer(),
    % 109 最良売気配数量4
    best_sell_volume_4 := integer(),
    % 110 最良売気配数量5
    best_sell_volume_5 := integer(),
    % 111 最良売気配数量6
    best_sell_volume_6 := integer(),
    % 112 最良売気配数量7
    best_sell_volume_7 := integer(),
    % 113 最良売気配数量8
    best_sell_volume_8 := integer(),
    % 114 最良売気配数量9
    best_sell_volume_9 := integer(),
    % 115 最良売気配数量10
    best_sell_volume_10 := integer(),
    % 116 最良買気配数量1
    best_buy_volume_1 := integer(),
    % 117 最良買気配数量2
    best_buy_volume_2 := integer(),
    % 118 最良買気配数量3
    best_buy_volume_3 := integer(),
    % 119 最良買気配数量4
    best_buy_volume_4 := integer(),
    % 120 最良買気配数量5
    best_buy_volume_5 := integer(),
    % 121 最良買気配数量6
    best_buy_volume_6 := integer(),
    % 122 最良買気配数量7
    best_buy_volume_7 := integer(),
    % 123 最良買気配数量8
    best_buy_volume_8 := integer(),
    % 124 最良買気配数量9
    best_buy_volume_9 := integer(),
    % 125 最良買気配数量10
    best_buy_volume_10 := integer(),
    % 126 売成行数量
    sell_order_volume := integer(),
    % 127 買成行数量
    buy_order_volume := integer(),
    % 128 OVER気配数量
    over_quote_volume := integer(),
    % 129 UNDER気配数量
    under_quote_volume := integer(),
    % 130 単位株数
    unit_shares := integer(),
    % 131 配当
    dividend := number(),
    % 132 配当落日 (YYYY/MM/DD)
    dividend_ex_date := klsn:binstr(),
    % 133 中配落日 (YYYY/MM/DD)
    mid_dividend_ex_date := klsn:binstr(),
    % 134 権利落日 (YYYY/MM/DD)
    rights_ex_date := klsn:binstr(),
    % 135 決算発表日 (YYYY/MM/DD)
    earnings_release_date := klsn:binstr(),
    % 136 PER
    per := number(),
    % 137 PBR
    pbr := number(),
    % 138 当日基準値
    base_value_today := number(),
    % 139 年初来高値
    year_high := number(),
    % 140 年初来安値
    year_low := number(),
    % 141 年初来高値日付 (YYYY/MM/DD)
    year_high_date := klsn:binstr(),
    % 142 年初来安値日付 (YYYY/MM/DD)
    year_low_date := klsn:binstr(),
    % 143 上場来高値
    all_time_high := number(),
    % 144 上場来安値
    all_time_low := number(),
    % 145 上場来高値日付 (YYYY/MM/DD)
    all_time_high_date := klsn:binstr(),
    % 146 上場来安値日付 (YYYY/MM/DD)
    all_time_low_date := klsn:binstr(),
    % 147 貸株金利
    lending_interest := number(),
    % 148 貸株金利適用日 (YYYY/MM/DD)
    lending_interest_date := klsn:binstr()
}.


-spec market_info_keys() -> [atom()].
market_info_keys() ->
    {ok, Concrete} = dialyzer_utils:get_core_from_beam(code:which(?MODULE)),
    {ok, Types} = dialyzer_utils:get_record_and_type_info(Concrete),
    MarketInfo = maps:get({type,market_info,0}, Types),
    lists:map(fun({type, _, map_field_exact, [{atom, _, Key}|_]})->
        Key
    end, element(4, element(3, element(1, MarketInfo)))).

