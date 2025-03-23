-module(kabue_status).

-export([
        status/0
      , health/0
      , kabue_rakuten_rss_market/0
    ]).

-export_type([
        status/0
    ]).

-type status() :: #{
        kabue_rakuten_rss_market := #{
            time_since_update := -1 | non_neg_integer()
          , time_since_webhook := -1 | non_neg_integer()
          , available_count := 0..500
        }
      , health := #{
            time_since_update := -1 | non_neg_integer()
        }
      , health := boolean()
      , timestamp := klsn_flux:timestamp()
    }.

-spec health() -> boolean().
health() ->
    maps:get(health, status()).

-spec status() -> status().
status() ->
    Status = #{
        kabue_rakuten_rss_market => kabue_rakuten_rss_market()
      , kabue_health => kabue_health()
      , timestamp => klsn_flux:timestamp()
    },
    Health = lists:all(fun(Bool) -> Bool end, [
        klsn_map:get([kabue_rakuten_rss_market, time_since_update], Status) < 60 * 60
      , klsn_map:get([kabue_rakuten_rss_market, time_since_update], Status) =/= -1
      , klsn_map:get([kabue_rakuten_rss_market, time_since_webhook], Status) < 60
      , klsn_map:get([kabue_rakuten_rss_market, time_since_webhook], Status) =/= -1
      , klsn_map:get([kabue_rakuten_rss_market, available_count], Status) > 0
      , klsn_map:get([kabue_health, time_since_update], Status) < 62
      , klsn_map:get([kabue_health, time_since_update], Status) =/= -1
    ]),
    Status#{
        health => Health
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

kabue_health() ->
    #{
        time_since_update => case
            {kabue_health:lookup(), klsn_flux:timestamp()}
        of
            {{value, #{timestamp:=Time0}}, Time1} ->
                (Time1 - Time0) div 1000000000;
            _ ->
                -1
        end
    }.

