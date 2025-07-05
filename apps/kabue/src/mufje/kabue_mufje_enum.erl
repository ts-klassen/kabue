-module(kabue_mufje_enum).

-export_type([
        exchange/0,
        security_type/0,
        side/0,
        cash_margin/0,
        margin_trade_type/0,
        deliv_type/0,
        fund_type/0,
        account_type/0,
        close_position_order/0,
        front_order_type/0,
        product/0,
        order_status/0,
        ranking_type/0,
        exchange_division/0,
        symbol/0,
        trigger_sec/0,
        under_over/0,
        after_hit_order_type/0,
        price_change_status/0,
        price_status/0,
        board_sign/0,
        instrument_type/0,
        ord_type/0,
        order_rec_type/0,
        detail_state/0
    ]).

-export([
        exchange/0,
        security_type/0,
        side/0,
        cash_margin/0,
        margin_trade_type/0,
        deliv_type/0,
        fund_type/0,
        account_type/0,
        close_position_order/0,
        front_order_type/0,
        product/0,
        order_status/0,
        ranking_type/0,
        exchange_division/0,
        symbol/0,
        trigger_sec/0,
        under_over/0,
        after_hit_order_type/0,
        price_change_status/0,
        price_status/0,
        board_sign/0,
        instrument_type/0,
        ord_type/0,
        order_rec_type/0,
        detail_state/0
    ]).

%% Type Definitions

-type exchange() :: tokyo       % 東証
                  | nagoya      % 名証
                  | fukusho     % 福証
                  | sapporo     % 札証
                  | sor         % SOR
                  | all_day     % 日通し
                  | daytime     % 日中
                  | nighttime   % 夜間
                  .

-type security_type() :: stock   % 株式
                       .

-type side() :: sell  % 売
              | buy   % 買
              .

-type cash_margin() :: spot          % 現物取引
                     | margin_new   % 新規 (信用取引)
                     | margin_repay % 返済 (信用取引)
                     .

-type margin_trade_type() :: regime           % 制度信用
                           | general_long     % 一般信用（長期）
                           | general_daytrade % 一般信用（デイトレ）
                           .

-type deliv_type() :: unspecified       % 指定なし
                    | deposit           % お預り金
                    | au_money_connect  % auマネーコネクト
                    .

-type fund_type() :: cash_sale         % 現物売の場合
                   | protection        % 保護
                   | credit_substitute % 信用代用
                   | credit_trading    % 信用取引
                   .

-type account_type() :: general      % 一般
                      | specific     % 特定
                      | corporation  % 法人
                      .

-type close_position_order() :: order_old_date_high_profit % 日付（古い順）、損益（高い順）
                              | order_old_date_low_profit  % 日付（古い順）、損益（低い順）
                              | order_new_date_high_profit % 日付（新しい順）、損益（高い順）
                              | order_new_date_low_profit  % 日付（新しい順）、損益（低い順）
                              | order_high_profit_old_date % 損益（高い順）、日付（古い順）
                              | order_high_profit_new_date % 損益（高い順）、日付（新しい順）
                              | order_low_profit_old_date  % 損益（低い順）、日付（古い順）
                              | order_low_profit_new_date  % 損益（低い順）、日付（新しい順）
                              .

-type front_order_type() :: market                 % 成行, <<"Price">>: 0, SOR可
                          | open_morning           % 寄成（前場）, <<"Price">>: 0
                          | open_afternoon         % 寄成（後場）, <<"Price">>: 0
                          | follow_morning         % 引成（前場）, <<"Price">>: 0
                          | follow_afternoon       % 引成（後場）, <<"Price">>: 0
                          | ioc_market             % IOC成行, <<"Price">>: 0
                          | limit                  % 指値, 発注したい金額, SOR可
                          | limit_open_morning     % 寄指（前場）, 発注したい金額
                          | limit_open_afternoon   % 寄指（後場）, 発注したい金額
                          | limit_follow_morning   % 引指（前場）, 発注したい金額
                          | limit_follow_afternoon % 引指（後場）, 発注したい金額
                          | nonlimit_open_morning  % 不成（前場）, 発注したい金額
                          | nonlimit_open_afternoon% 不成（後場）, 発注したい金額
                          | ioc_limit              % IOC指値, 発注したい金額
                          | stop                   % 逆指値, 指定なし (AfterHitPriceで指定)
                          .

-type product() :: all      % すべて
                 | spot     % 現物
                 | credit   % 信用
                 | futures  % 先物
                 | op       % OP
                 .

-type order_status() :: pending    % 待機（発注待機）
                      | sending    % 処理中（発注送信中）
                      | processed  % 処理済（発注済・訂正済）
                      | canceling  % 訂正取消送信中
                      | finished   % 終了（発注エラー・取消済・全約定・失効・期限切れ）
                      .

-type ranking_type() :: rising_rate             % 値上がり率（デフォルト）
                      | falling_rate            % 値下がり率
                      | trading_volume_top      % 売買高上位
                      | trading_amount          % 売買代金
                      | tick_count              % TICK回数
                      | volume_spike            % 売買高急増
                      | amount_spike            % 売買代金急増
                      | credit_short_increase   % 信用売残増
                      | credit_short_decrease   % 信用売残減
                      | credit_buy_increase     % 信用買残増
                      | credit_buy_decrease     % 信用買残減
                      | credit_high_ratio       % 信用高倍率
                      | credit_low_ratio        % 信用低倍率
                      | industry_rising_rate    % 業種別値上がり率
                      | industry_falling_rate   % 業種別値下がり率
                      .

-type exchange_division() :: all_market    % 全市場（デフォルト）
                           | tse_total    % 東証全体
                           | tse_prime    % 東証プライム
                           | tse_standard % 東証スタンダード
                           | growth250     % グロース250
                           | nagoya        % 名証
                           | fukusho       % 福証
                           | sapporo       % 札証
                           .

-type symbol() :: usd_jpy   % USD/JPY
                | eur_jpy   % EUR/JPY
                | gbp_jpy   % GBP/JPY
                | aud_jpy   % AUD/JPY
                | chf_jpy   % CHF/JPY
                | cad_jpy   % CAD/JPY
                | nzd_jpy   % NZD/JPY
                | zar_jpy   % ZAR/JPY
                | eur_usd   % EUR/USD
                | gbp_usd   % GBP/USD
                | aud_usd   % AUD/USD
                .

-type trigger_sec() :: order      % 発注銘柄
                     | nk225      % NK225指数
                     | topix      % TOPIX指数
                     .

-type under_over() :: under     % 以下
                    | over     % 以上
                    .

-type after_hit_order_type() :: market    % 成行
                              | limit     % 指値
                              | nonlimit  % 不成
                              .

-type price_change_status() :: event_none % 事象なし
                             | unchanged % 変わらず
                             | up % UP
                             | down % DOWN
                             | first_after_pause % 中断板寄り後の初値
                             | regular_close % ザラバ引け
                             | order_close % 板寄り引け
                             | pause_close % 中断引け
                             | down_close % ダウン引け
                             | reversal_close % 逆転終値
                             | special_close % 特別気配引け
                             | temp_hold_close % 一時留保引け
                             | trading_stop_close % 売買停止引け
                             | circuit_breaker_close % サーキットブレーカ引け
                             | dynamic_circuit_breaker_close % ダイナミックサーキットブレーカ引け
                             .

-type price_status() :: price % 現値
                       | discontinuous_walking % 不連続歩み
                       | order_matching % 板寄せ
                       | system_error % システム障害
                       | pause % 中断
                       | trading_stop % 売買停止
                       | trading_stop_system_release % 売買停止・システム停止解除
                       | close % 終値
                       | system_stop % システム停止
                       | approximate % 概算値
                       | reference % 参考値
                       | circuit_break_ongoing % サーキットブレイク実施中
                       | system_error_release % システム障害解除
                       | circuit_break_release % サーキットブレイク解除
                       | pause_release % 中断解除
                       | temporary_hold % 一時留保中
                       | temporary_hold_release % 一時留保解除
                       | file_error % ファイル障害
                       | file_error_release % ファイル障害解除
                       | spread_strategy % Spread/Strategy
                       | dynamic_circuit_breaker_trigger % ダイナミックサーキットブレイク発動
                       | dynamic_circuit_breaker_release % ダイナミックサーキットブレイク解除
                       | matched_order % 板寄せ約定
                       .

-type board_sign() :: event_none % 事象なし
              | general_quote % 一般気配
              | special_quote % 特別気配
              | caution_quote % 注意気配
              | pre_open_quote % 寄前気配
              | pre_stop_special_quote % 停止前特別気配
              | post_close_quote % 引け後気配
              | pre_quote_match_no_point % 寄前気配約定成立ポイントなし
              | pre_quote_match_with_point % 寄前気配約定成立ポイントあり
              | continuous_quote % 連続約定気配
              | continuous_quote_pre_stop % 停止前の連続約定気配
              | upward_quote % 買い上がり売り下がり中
              .

-type instrument_type() :: index % 指数
                         | spot % 現物
                         | nikkei225_future % 日経225先物
                         | nikkei225_op % 日経225OP
                         | topix_future % TOPIX先物
                         | jpx400_future % JPX400先物
                         | ny_dow % NYダウ
                         | nikkei_average_vi % 日経平均VI
                         | growth250_future % グロース250先物
                         | topix_reit % TOPIX_REIT
                         | topix_core30 % TOPIX CORE30
                         | nikkei225_mini_future % 日経平均225ミニ先物
                         | topix_mini_future % TOPIXミニ先物
                         .

%% ------------------------------------------------------------------
%% New enumerations for order executions
%% ------------------------------------------------------------------

-type ord_type() :: continuous      % ザラバ
                 | open            % 寄り
                 | close           % 引け
                 | nonlimit        % 不成
                 | counter_limit   % 対当指値
                 | ioc.            % IOC

-type order_rec_type() :: accepted   % 受付
                       | carryover   % 繰越
                       | expired     % 期限切れ
                       | sent        % 発注
                       | modified    % 訂正
                       | canceled    % 取消
                       | lapsed      % 失効
                       | executed.   % 約定

-type detail_state() :: wait         % 待機（発注待機）
                     | processing    % 処理中（発注送信中・訂正送信中・取消送信中）
                     | processed     % 処理済（発注済・訂正済・取消済・全約定・期限切れ）
                     | error         % エラー
                     | deleted.      % 削除済み


%% Function Definitions

-spec exchange() -> maps:map(exchange(), integer()).
exchange() ->
    #{
        tokyo      => 1,
        nagoya     => 3,
        fukusho    => 5,
        sapporo    => 6,
        sor        => 9,
        all_day    => 2,
        daytime    => 23,
        nighttime  => 24
    }.

-spec security_type() -> maps:map(security_type(), integer()).
security_type() ->
    #{
        stock => 1
    }.

-spec side() -> maps:map(side(), klsn:binstr()).
side() ->
    #{
        sell => <<"1">>,
        buy  => <<"2">>
    }.

-spec cash_margin() -> maps:map(cash_margin(), integer()).
cash_margin() ->
    #{
        spot         => 1,
        margin_new   => 2,
        margin_repay => 3
    }.

-spec margin_trade_type() -> maps:map(margin_trade_type(), integer()).
margin_trade_type() ->
    #{
        regime           => 1,
        general_long     => 2,
        general_daytrade => 3
    }.

-spec deliv_type() -> maps:map(deliv_type(), integer()).
deliv_type() ->
    #{
        unspecified      => 0,
        deposit          => 2,
        au_money_connect => 3
    }.

-spec fund_type() -> maps:map(fund_type(), klsn:binstr()).
fund_type() ->
    #{
        cash_sale         => <<"  ">>,  % 半角スペース2つ
        protection        => <<"02">>,
        credit_substitute => <<"AA">>,
        credit_trading    => <<"11">>
    }.

-spec account_type() -> maps:map(account_type(), integer()).
account_type() ->
    #{
        general     => 2,
        specific    => 4,
        corporation => 12
    }.

-spec close_position_order() -> maps:map(close_position_order(), integer()).
close_position_order() ->
    #{
        order_old_date_high_profit => 0,
        order_old_date_low_profit  => 1,
        order_new_date_high_profit => 2,
        order_new_date_low_profit  => 3,
        order_high_profit_old_date => 4,
        order_high_profit_new_date => 5,
        order_low_profit_old_date  => 6,
        order_low_profit_new_date  => 7
    }.

-spec front_order_type() -> maps:map(front_order_type(), integer()).
front_order_type() ->
    #{
        market                 => 10,
        open_morning           => 13,
        open_afternoon         => 14,
        follow_morning         => 15,
        follow_afternoon       => 16,
        ioc_market             => 17,
        limit                  => 20,
        limit_open_morning     => 21,
        limit_open_afternoon   => 22,
        limit_follow_morning   => 23,
        limit_follow_afternoon => 24,
        nonlimit_open_morning  => 25,
        nonlimit_open_afternoon=> 26,
        ioc_limit              => 27,
        stop                   => 30
    }.

%% OrdType -----------------------------------------------------------

-spec ord_type() -> maps:map(ord_type(), integer()).
ord_type() ->
    #{
        continuous    => 1,
        open          => 2,
        close         => 3,
        nonlimit      => 4,
        counter_limit => 5,
        ioc           => 6
    }.

%% Order execution RecType ------------------------------------------

-spec order_rec_type() -> maps:map(order_rec_type(), integer()).
order_rec_type() ->
    #{
        accepted   => 1,
        carryover  => 2,
        expired    => 3,
        sent       => 4,
        modified   => 5,
        canceled   => 6,
        lapsed     => 7,
        executed   => 8
    }.

%% Detail State ------------------------------------------------------

-spec detail_state() -> maps:map(detail_state(), integer()).
detail_state() ->
    #{
        wait       => 1,
        processing => 2,
        processed  => 3,
        error      => 4,
        deleted    => 5
    }.

-spec product() -> maps:map(product(), klsn:binstr()).
product() ->
    #{
        all     => <<"0">>,
        spot    => <<"1">>,
        credit  => <<"2">>,
        futures => <<"3">>,
        op      => <<"4">>
    }.

-spec order_status() -> maps:map(order_status(), klsn:binstr()).
order_status() ->
    #{
        pending   => <<"1">>,
        sending   => <<"2">>,
        processed => <<"3">>,
        canceling => <<"4">>,
        finished  => <<"5">>
    }.

-spec ranking_type() -> maps:map(ranking_type(), klsn:binstr()).
ranking_type() ->
    #{
        rising_rate           => <<"1">>,
        falling_rate          => <<"2">>,
        trading_volume_top    => <<"3">>,
        trading_amount        => <<"4">>,
        tick_count            => <<"5">>,
        volume_spike          => <<"6">>,
        amount_spike          => <<"7">>,
        credit_short_increase => <<"8">>,
        credit_short_decrease => <<"9">>,
        credit_buy_increase   => <<"10">>,
        credit_buy_decrease   => <<"11">>,
        credit_high_ratio     => <<"12">>,
        credit_low_ratio      => <<"13">>,
        industry_rising_rate  => <<"14">>,
        industry_falling_rate => <<"15">>
    }.

-spec exchange_division() -> maps:map(exchange_division(), klsn:binstr()).
exchange_division() ->
    #{
        all_market    => <<"ALL">>,
        tse_total     => <<"T">>,
        tse_prime     => <<"TP">>,
        tse_standard  => <<"TS">>,
        growth250     => <<"TG">>,
        nagoya        => <<"M">>,
        fukusho       => <<"FK">>,
        sapporo       => <<"S">>
    }.

-spec symbol() -> maps:map(symbol(), klsn:binstr()).
symbol() ->
    #{
        usd_jpy => <<"usdjpy">>,
        eur_jpy => <<"eurjpy">>,
        gbp_jpy => <<"gbpjpy">>,
        aud_jpy => <<"audjpy">>,
        chf_jpy => <<"chfjpy">>,
        cad_jpy => <<"cadjpy">>,
        nzd_jpy => <<"nzdjpy">>,
        zar_jpy => <<"zarjpy">>,
        eur_usd => <<"eurusd">>,
        gbp_usd => <<"gbpusd">>,
        aud_usd => <<"audusd">>
    }.

-spec trigger_sec() -> maps:map(trigger_sec(), integer()).
trigger_sec() ->
    #{
        order => 1,
        nk225 => 2,
        topix => 3
    }.

-spec under_over() -> maps:map(under_over(), integer()).
under_over() ->
    #{
        under => 1,
        over => 2
    }.

-spec after_hit_order_type() -> maps:map(after_hit_order_type(), integer()).
after_hit_order_type() ->
    #{
        market   => 1,
        limit    => 2,
        nonlimit => 3
    }.

-spec price_change_status() -> maps:map(price_change_status(), klsn:binstr()).
price_change_status() ->
    #{
        null => <<>>, % Not documented
        event_none => <<"0000">>,
        unchanged => <<"0056">>,
        up => <<"0057">>,
        down => <<"0058">>,
        first_after_pause => <<"0059">>,
        regular_close => <<"0060">>,
        order_close => <<"0061">>,
        pause_close => <<"0062">>,
        down_close => <<"0063">>,
        reversal_close => <<"0064">>,
        special_close => <<"0066">>,
        temp_hold_close => <<"0067">>,
        trading_stop_close => <<"0068">>,
        circuit_breaker_close => <<"0069">>,
        dynamic_circuit_breaker_close => <<"0431">>
    }.

-spec price_status() -> maps:map(price_status(), integer()).
price_status() ->
    #{
        null => -1, % Not documented
        price => 1,
        discontinuous_walking => 2,
        order_matching => 3,
        system_error => 4,
        pause => 5,
        trading_stop => 6,
        trading_stop_system_release => 7,
        close => 8,
        system_stop => 9,
        approximate => 10,
        reference => 11,
        circuit_break_ongoing => 12,
        system_error_release => 13,
        circuit_break_release => 14,
        pause_release => 15,
        temporary_hold => 16,
        temporary_hold_release => 17,
        file_error => 18,
        file_error_release => 19,
        spread_strategy => 20,
        dynamic_circuit_breaker_trigger => 21,
        dynamic_circuit_breaker_release => 22,
        matched_order => 23
    }.

-spec board_sign() -> maps:map(board_sign(), klsn:binstr()).
board_sign() ->
    #{
        event_none => <<"0000">>,
        general_quote => <<"0101">>,
        special_quote => <<"0102">>,
        caution_quote => <<"0103">>,
        pre_open_quote => <<"0107">>,
        pre_stop_special_quote => <<"0108">>,
        post_close_quote => <<"0109">>,
        pre_quote_match_no_point => <<"0116">>,
        pre_quote_match_with_point => <<"0117">>,
        continuous_quote => <<"0118">>,
        continuous_quote_pre_stop => <<"0119">>,
        upward_quote => <<"0120">>
    }.

-spec instrument_type() -> maps:map(instrument_type(), integer()).
instrument_type() ->
    #{
        index => 0,
        spot => 1,
        nikkei225_future => 101,
        nikkei225_op => 103,
        topix_future => 107,
        jpx400_future => 121,
        ny_dow => 144,
        nikkei_average_vi => 145,
        growth250_future => 154,
        topix_reit => 155,
        topix_core30 => 171,
        nikkei225_mini_future => 901,
        topix_mini_future => 907
    }.

%% EOF
