-module(kabue_status).

-export([
        status/0
      , health/0
      , kabue_rakuten_rss_market/0
    ]).

health() ->
    Status = status(),
    lists:all(fun(Bool) -> Bool end, [
        klsn_map:get([kabue_rakuten_rss_market, time_since_update], Status) < 60 * 60
      , klsn_map:get([kabue_rakuten_rss_market, time_since_update], Status) =/= -1
      , klsn_map:get([kabue_rakuten_rss_market, time_since_webhook], Status) < 60
      , klsn_map:get([kabue_rakuten_rss_market, time_since_webhook], Status) =/= -1
      , klsn_map:get([kabue_rakuten_rss_market, available_count], Status) > 0
    ]).

status() ->
    #{
        kabue_rakuten_rss_market => kabue_rakuten_rss_market()
    }.

kabue_rakuten_rss_market() ->
    #{
        time_since_update => case
            {kabue_rakuten_rss_market:last_updated_at(), klsn_flux:timestamp()}
        of
            {{value, Time0}, Time1} ->
                (Time1 - Time0) div 1000000000;
            _ ->
                -1
        end
      , time_since_webhook => case
            {kabue_rakuten_rss_market:last_webhook_at(), klsn_flux:timestamp()}
        of
            {{value, Time0}, Time1} ->
                (Time1 - Time0) div 1000000000;
            _ ->
                -1
        end
      , available_count => kabue_rakuten_rss_market:available_count()
    }.

