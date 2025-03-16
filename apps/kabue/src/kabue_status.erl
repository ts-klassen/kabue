-module(kabue_status).

-export([
        status/0
      , health/0
      , kabue_rakuten_rss_market/0
    ]).

health() ->
    Status = status(),
    lists:all(fun(Bool) -> Bool end, [
        klsn_map:get([kabue_rakuten_rss_market, idle], Status) < 60
      , klsn_map:get([kabue_rakuten_rss_market, idle], Status) =/= -1
      , klsn_map:get([kabue_rakuten_rss_market, available_count], Status) > 0
    ]).

status() ->
    #{
        kabue_rakuten_rss_market => kabue_rakuten_rss_market()
    }.

kabue_rakuten_rss_market() ->
    #{
        idle => case
            {kabue_rakuten_rss_market:last_updated_at(), os:system_time()}
        of
            {{value, Time0}, Time1} ->
                (Time1 - Time0) div 1000000000;
            _ ->
                -1
        end
      , available_count => kabue_rakuten_rss_market:available_count()
    }.

