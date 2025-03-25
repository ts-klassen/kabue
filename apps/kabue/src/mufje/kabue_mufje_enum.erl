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
        symbol/0
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
        symbol/0
    ]).

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
                           | tose_total    % 東証全体
                           | tose_prime    % 東証プライム
                           | tose_standard % 東証スタンダード
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
        tose_total    => <<"T">>,
        tose_prime    => <<"TP">>,
        tose_standard => <<"TS">>,
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
