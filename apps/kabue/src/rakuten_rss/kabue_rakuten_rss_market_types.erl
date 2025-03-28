-module(kabue_rakuten_rss_market_types).
% https://marketspeed.jp/guide/manual/ms2rss_function.pdf

-export_type([
        jpx_market_info/0
    ]).

-export([
        jpx_market_info_keys/0
      , jpx_market_info_types/0
      , convert/3
    ]).

-type date() :: {integer(), 1..12, 1..31}.

-type time() :: {0..59, 0..59}.

-type detail_time() :: {0..59, 0..59, 0..59}.

-type jpx_market_info() :: #{
    % 引数として指定した ticker
    ticker => klsn:maybe(klsn:binstr())
    % 1 銘柄コード
  , ticker_code => klsn:maybe(klsn:binstr())
    % 2 市場コード
  , market_code => klsn:maybe(klsn:binstr())
    % 3 銘柄名称
  , ticker_name => klsn:maybe(klsn:binstr())
    % 4 市場名称
  , market_name => klsn:maybe(klsn:binstr())
    % 5 市場部名称
  , market_section_name => klsn:maybe(klsn:binstr())
    % 6 市場部略称
  , market_section_abbr => klsn:maybe(klsn:binstr())
    % 7 現在日付 (YYYY/MM/DD)
  , current_date => klsn:maybe(date())
    % 8 現在値
  , current_price => klsn:maybe(float())
    % 9 現在値時刻 (HH:MM)
  , current_price_time => klsn:maybe(time())
    % 10 現在値詳細時刻 (HH:MM:SS)
  , current_price_detail_time => klsn:maybe(detail_time())
    % 11 現在値ティック
  , current_tick => klsn:maybe(klsn:binstr())
    % 12 現在値フラグ
  , current_price_flag => klsn:maybe(klsn:binstr())
    % 13 前日比
  , previous_diff => klsn:maybe(float())
    % 14 前日比率
  , previous_ratio => klsn:maybe(float())
    % 15 前日終値
  , previous_close => klsn:maybe(float())
    % 16 前日日付 (YYYY/MM/DD)
  , previous_date => klsn:maybe(date())
    % 17 前日終値フラグ
  , previous_close_flag => klsn:maybe(klsn:binstr())
    % 18 歩み1
  , tick1 => klsn:maybe(float())
    % 19 歩み2
  , tick2 => klsn:maybe(float())
    % 20 歩み3
  , tick3 => klsn:maybe(float())
    % 21 歩み4
  , tick4 => klsn:maybe(float())
    % 22 歩み1詳細時刻 (HH:MM:SS)
  , tick1_time => klsn:maybe(detail_time())
    % 23 歩み2詳細時刻 (HH:MM:SS)
  , tick2_time => klsn:maybe(detail_time())
    % 24 歩み3詳細時刻 (HH:MM:SS)
  , tick3_time => klsn:maybe(detail_time())
    % 25 歩み4詳細時刻 (HH:MM:SS)
  , tick4_time => klsn:maybe(detail_time())
    % 26 出来高
  , volume => klsn:maybe(integer())
    % 27 売買代金
  , trade_value => klsn:maybe(integer())
    % 28 出来高加重平均
  , volume_weighted_avg => klsn:maybe(float())
    % 29 時価総額
  , market_cap => klsn:maybe(integer())
    % 30 始値
  , open_price => klsn:maybe(float())
    % 31 高値
  , high_price => klsn:maybe(float())
    % 32 安値
  , low_price => klsn:maybe(float())
    % 33 始値時刻 (HH:MM)
  , open_time => klsn:maybe(time())
    % 34 高値時刻 (HH:MM)
  , high_time => klsn:maybe(time())
    % 35 安値時刻 (HH:MM)
  , low_time => klsn:maybe(time())
    % 36 始値詳細時刻 (HH:MM:SS)
  , open_detail_time => klsn:maybe(detail_time())
    % 37 高値詳細時刻 (HH:MM:SS)
  , high_detail_time => klsn:maybe(detail_time())
    % 38 安値詳細時刻 (HH:MM:SS)
  , low_detail_time => klsn:maybe(detail_time())
    % 39 前場始値
  , morning_open => klsn:maybe(float())
    % 40 前場高値
  , morning_high => klsn:maybe(float())
    % 41 前場安値
  , morning_low => klsn:maybe(float())
    % 42 前場終値
  , morning_close => klsn:maybe(float())
    % 43 前場出来高
  , morning_volume => klsn:maybe(integer())
    % 44 後場始値
  , afternoon_open => klsn:maybe(float())
    % 45 後場高値
  , afternoon_high => klsn:maybe(float())
    % 46 後場安値
  , afternoon_low => klsn:maybe(float())
    % 47 前場始値時刻 (HH:MM)
  , morning_open_time => klsn:maybe(time())
    % 48 前場高値時刻 (HH:MM)
  , morning_high_time => klsn:maybe(time())
    % 49 前場安値時刻 (HH:MM)
  , morning_low_time => klsn:maybe(time())
    % 50 前場終値時刻 (HH:MM)
  , morning_close_time => klsn:maybe(time())
    % 51 前場出来高時刻 (HH:MM)
  , morning_volume_time => klsn:maybe(time())
    % 52 後場始値時刻 (HH:MM)
  , afternoon_open_time => klsn:maybe(time())
    % 53 後場高値時刻 (HH:MM)
  , afternoon_high_time => klsn:maybe(time())
    % 54 後場安値時刻 (HH:MM)
  , afternoon_low_time => klsn:maybe(time())
    % 55 最良売気配値
  , best_sell_price => klsn:maybe(float())
    % 56 最良買気配値
  , best_buy_price => klsn:maybe(float())
    % 57 最良売気配数量
  , best_sell_volume => klsn:maybe(integer())
    % 58 最良買気配数量
  , best_buy_volume => klsn:maybe(integer())
    % 59 最良売気配時刻 (HH:MM)
  , best_sell_time => klsn:maybe(time())
    % 60 最良買気配時刻 (HH:MM)
  , best_buy_time => klsn:maybe(time())
    % 61 最良売気配詳細時刻 (HH:MM:SS)
  , best_sell_detail_time => klsn:maybe(detail_time())
    % 62 最良買気配詳細時刻 (HH:MM:SS)
  , best_buy_detail_time => klsn:maybe(detail_time())
    % 63 特別売気配フラグ
  , special_sell_flag => klsn:maybe(klsn:binstr())
    % 64 特別買気配フラグ
  , special_buy_flag => klsn:maybe(klsn:binstr())
    % 65 信用貸借区分 (1:貸借、2:信用)
  , credit_lending_type => klsn:maybe(integer())
    % 66 逆日歩
  , reverse_interest => klsn:maybe(float())
    % 67 逆日歩更新日付 (YYYY/MM/DD)
  , reverse_interest_date => klsn:maybe(date())
    % 68 信用売残
  , credit_sell_balance => klsn:maybe(integer())
    % 69 信用売残前週比
  , credit_sell_prev_week_diff => klsn:maybe(integer())
    % 70 信用買残
  , credit_buy_balance => klsn:maybe(integer())
    % 71 信用買残前週比
  , credit_buy_prev_week_diff => klsn:maybe(integer())
    % 72 信用倍率
  , credit_ratio => klsn:maybe(float())
    % 73 証金残更新日付 (YYYY/MM/DD)
  , margin_balance_update_date => klsn:maybe(date())
    % 74 新規貸株
  , new_lending_stock => klsn:maybe(integer())
    % 75 新規融資
  , new_financing => klsn:maybe(integer())
    % 76 返済貸株
  , repaid_lending_stock => klsn:maybe(integer())
    % 77 返済融資
  , repaid_financing => klsn:maybe(integer())
    % 78 残高貸株
  , balance_lending_stock => klsn:maybe(integer())
    % 79 残高融資
  , balance_financing => klsn:maybe(integer())
    % 80 残高差引
  , balance_diff => klsn:maybe(integer())
    % 81 前日比貸株
  , prev_lending_diff => klsn:maybe(integer())
    % 82 前日比融資
  , prev_financing_diff => klsn:maybe(integer())
    % 83 前日比差引
  , net_diff => klsn:maybe(integer())
    % 84 回転日数
  , turnover_days => klsn:maybe(float())
    % 85 貸借倍率
  , lending_ratio => klsn:maybe(float())
    % 86 最良売気配値1
  , best_sell_price_1 => klsn:maybe(float())
    % 87 最良売気配値2
  , best_sell_price_2 => klsn:maybe(float())
    % 88 最良売気配値3
  , best_sell_price_3 => klsn:maybe(float())
    % 89 最良売気配値4
  , best_sell_price_4 => klsn:maybe(float())
    % 90 最良売気配値5
  , best_sell_price_5 => klsn:maybe(float())
    % 91 最良売気配値6
  , best_sell_price_6 => klsn:maybe(float())
    % 92 最良売気配値7
  , best_sell_price_7 => klsn:maybe(float())
    % 93 最良売気配値8
  , best_sell_price_8 => klsn:maybe(float())
    % 94 最良売気配値9
  , best_sell_price_9 => klsn:maybe(float())
    % 95 最良売気配値10
  , best_sell_price_10 => klsn:maybe(float())
    % 96 最良買気配値1
  , best_buy_price_1 => klsn:maybe(float())
    % 97 最良買気配値2
  , best_buy_price_2 => klsn:maybe(float())
    % 98 最良買気配値3
  , best_buy_price_3 => klsn:maybe(float())
    % 99 最良買気配値4
  , best_buy_price_4 => klsn:maybe(float())
    % 100 最良買気配値5
  , best_buy_price_5 => klsn:maybe(float())
    % 101 最良買気配値6
  , best_buy_price_6 => klsn:maybe(float())
    % 102 最良買気配値7
  , best_buy_price_7 => klsn:maybe(float())
    % 103 最良買気配値8
  , best_buy_price_8 => klsn:maybe(float())
    % 104 最良買気配値9
  , best_buy_price_9 => klsn:maybe(float())
    % 105 最良買気配値10
  , best_buy_price_10 => klsn:maybe(float())
    % 106 最良売気配数量1
  , best_sell_volume_1 => klsn:maybe(integer())
    % 107 最良売気配数量2
  , best_sell_volume_2 => klsn:maybe(integer())
    % 108 最良売気配数量3
  , best_sell_volume_3 => klsn:maybe(integer())
    % 109 最良売気配数量4
  , best_sell_volume_4 => klsn:maybe(integer())
    % 110 最良売気配数量5
  , best_sell_volume_5 => klsn:maybe(integer())
    % 111 最良売気配数量6
  , best_sell_volume_6 => klsn:maybe(integer())
    % 112 最良売気配数量7
  , best_sell_volume_7 => klsn:maybe(integer())
    % 113 最良売気配数量8
  , best_sell_volume_8 => klsn:maybe(integer())
    % 114 最良売気配数量9
  , best_sell_volume_9 => klsn:maybe(integer())
    % 115 最良売気配数量10
  , best_sell_volume_10 => klsn:maybe(integer())
    % 116 最良買気配数量1
  , best_buy_volume_1 => klsn:maybe(integer())
    % 117 最良買気配数量2
  , best_buy_volume_2 => klsn:maybe(integer())
    % 118 最良買気配数量3
  , best_buy_volume_3 => klsn:maybe(integer())
    % 119 最良買気配数量4
  , best_buy_volume_4 => klsn:maybe(integer())
    % 120 最良買気配数量5
  , best_buy_volume_5 => klsn:maybe(integer())
    % 121 最良買気配数量6
  , best_buy_volume_6 => klsn:maybe(integer())
    % 122 最良買気配数量7
  , best_buy_volume_7 => klsn:maybe(integer())
    % 123 最良買気配数量8
  , best_buy_volume_8 => klsn:maybe(integer())
    % 124 最良買気配数量9
  , best_buy_volume_9 => klsn:maybe(integer())
    % 125 最良買気配数量10
  , best_buy_volume_10 => klsn:maybe(integer())
    % 126 売成行数量
  , sell_order_volume => klsn:maybe(integer())
    % 127 買成行数量
  , buy_order_volume => klsn:maybe(integer())
    % 128 OVER気配数量
  , over_quote_volume => klsn:maybe(integer())
    % 129 UNDER気配数量
  , under_quote_volume => klsn:maybe(integer())
    % 130 単位株数
  , unit_shares => klsn:maybe(integer())
    % 131 配当
  , dividend => klsn:maybe(float())
    % 132 配当落日 (YYYY/MM/DD)
  , dividend_ex_date => klsn:maybe(date())
    % 133 中配落日 (YYYY/MM/DD)
  , mid_dividend_ex_date => klsn:maybe(date())
    % 134 権利落日 (YYYY/MM/DD)
  , rights_ex_date => klsn:maybe(date())
    % 135 決算発表日 (YYYY/MM/DD)
  , earnings_release_date => klsn:maybe(date())
    % 136 PER
  , per => klsn:maybe(float())
    % 137 PBR
  , pbr => klsn:maybe(float())
    % 138 当日基準値
  , base_value_today => klsn:maybe(integer())
    % 139 年初来高値
  , year_high => klsn:maybe(float())
    % 140 年初来安値
  , year_low => klsn:maybe(float())
    % 141 年初来高値日付 (YYYY/MM/DD)
  , year_high_date => klsn:maybe(date())
    % 142 年初来安値日付 (YYYY/MM/DD)
  , year_low_date => klsn:maybe(date())
    % 143 上場来高値
  , all_time_high => klsn:maybe(float())
    % 144 上場来安値
  , all_time_low => klsn:maybe(float())
    % 145 上場来高値日付 (YYYY/MM/DD)
  , all_time_high_date => klsn:maybe(date())
    % 146 上場来安値日付 (YYYY/MM/DD)
  , all_time_low_date => klsn:maybe(date())
    % 147 貸株金利
  , lending_interest => klsn:maybe(float())
    % 148 貸株金利適用日 (YYYY/MM/DD)
  , lending_interest_date => klsn:maybe(date())
}.


-spec jpx_market_info_keys() -> [atom()].
jpx_market_info_keys() ->
    {ok, Concrete} = dialyzer_utils:get_core_from_beam(code:which(?MODULE)),
    {ok, Types} = dialyzer_utils:get_record_and_type_info(Concrete),
    MarketInfo = maps:get({type,jpx_market_info,0}, Types),
    lists:map(fun({type, _, _, [{atom, _, Key}|_]})->
        Key
    end, element(4, element(3, element(1, MarketInfo)))).


-spec jpx_market_info_types() -> [{string | int | float, atom()}].
jpx_market_info_types() ->
    {ok, Concrete} = dialyzer_utils:get_core_from_beam(code:which(?MODULE)),
    {ok, Types} = dialyzer_utils:get_record_and_type_info(Concrete),
    MarketInfo = maps:get({type,jpx_market_info,0}, Types),
    lists:map(fun({type, _, _, [{atom, _, Key}|T]})->
        [{remote_type,_,[{atom,_,klsn},{atom,_,maybe},MaybeArgs]}] = T,
        Type = case MaybeArgs of
            [{remote_type,_,[{atom,_,klsn},{atom,_,binstr},[]]}] ->
                string;
            [{type,_,integer,[]}] ->
                int;
            [{type,_,float,[]}] ->
                float;
            [{user_type,_,date,[]}] ->
                date;
            [{user_type,_,time,[]}] ->
                time;
            [{user_type,_,detail_time,[]}] ->
                detail_time
        end,
        {Type, Key}
    end, element(4, element(3, element(1, MarketInfo)))).

-spec convert(string, klsn:binstr(), atom()) -> klsn:maybe(klsn:binstr());
             (int, klsn:binstr(), atom()) -> klsn:maybe(integer());
             (float, klsn:binstr(), atom()) -> klsn:maybe(float()).
convert(_, _, <<>>) -> none;
convert(_, _, <<" ">>) -> none;
convert(_, _, <<"-">>) -> none;
convert(date, _, <<"  /  /  ">>) -> none;
convert(time, _, <<"  :  ">>) -> none;
convert(detail_time, _, <<"  :  :  ">>) -> none;
convert(Type, Key, String) ->
    try
        case {Type, String} of
            {string, _} ->
                {value, iolist_to_binary(String)};
            {int, _} ->
                {value, binary_to_integer(String)};
            {float, _} ->
                try binary_to_integer(String) of
                    Int -> {value, Int + 0.0}
                catch error:badarg ->
                    {value, binary_to_float(String)}
                end;
            {date, <<Y:4/binary, $/, M:2/binary, $/, D:2/binary>>} ->
                {value, {
                    binary_to_integer(Y)
                  , binary_to_integer(M)
                  , binary_to_integer(D)
                }};
            {time, <<H:2/binary, $:, M:2/binary>>} ->
                {value, {
                    binary_to_integer(H)
                  , binary_to_integer(M)
                }};
            {detail_time, <<H:2/binary, $:, M:2/binary, $:, S:2/binary>>} ->
                {value, {
                    binary_to_integer(H)
                  , binary_to_integer(M)
                  , binary_to_integer(S)
                }};
            _ ->
                error(badarg)
        end
    catch
        error:badarg ->
            erlang:error(kabue_rakuten_rss_market_types_convert_error, [
                             Type, Key, String
                         ])
    end.


