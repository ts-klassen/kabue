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
      , kabue_health := #{
            time_since_update := -1 | non_neg_integer()
        }
      , kabue_mufje_rest_apic := #{
            success := boolean()
          , is_httpc_ok := boolean()
          , http_status_code => 200..599
          , kabusapi_code => integer()
          , kabusapi_message => klsn:binstr()
          , is_login_required := boolean()
          , is_available_time := boolean()
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
      , kabue_mufje_rest_apic => kabue_mufje_rest_apic()
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
      , klsn_map:get([kabue_mufje_rest_apic, success], Status)
            orelse not klsn_map:get([kabue_mufje_rest_apic, is_available_time], Status)
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

kabue_mufje_rest_apic() ->
    try kabue_mufje_rest_apic_() catch
        throw:{?MODULE, left, Left} ->
            klsn_map:filter(#{
                success => {value, false}
              , is_httpc_ok => case Left of
                    #{ res := {ok, _}} ->
                        {value, true};
                    _ ->
                        {value, false}
                end
              , http_status_code => case Left of
                    #{ status := Status } ->
                        {value, Status};
                    _ ->
                        none
                end
              , kabusapi_code => case Left of
                    #{ code := Code } ->
                        {value, Code};
                    _ ->
                        none
                end
              , kabusapi_message => case Left of
                    #{ payload := #{<<"Message">>:=Msg} } ->
                        {value, Msg};
                    _ ->
                        none
                end
              , is_login_required => case Left of
                    #{ code := 4001007 } ->
                        {value, true};
                    #{ code := 4001017 } ->
                        {value, true};
                    #{ resp := {error,socket_closed_remotely} } ->
                        {value, true};
                    _ ->
                        {value, false}
                end
              , is_available_time => {value, is_mufje_available_time()}
            })
    end.
kabue_mufje_rest_apic_() ->
    Do = fun({right, Right}) -> Right;
            ({left, Left}) -> throw({?MODULE, left, Left})
    end,
    Token = Do(kabue_mufje_rest_apic:token(#{ mode => test })),
    Do(kabue_mufje_rest_apic:ranking(#{}, #{
        mode => test
      , token => Token
    })),
    #{
        success => true
      , is_httpc_ok => true
      , http_status_code => 200
      , is_login_required => false
      , is_available_time => is_mufje_available_time()
    }.

is_mufje_available_time() ->
    case erlang:time() of
        {6, M, _} when 15 =< M, M < 30 -> false;
        _ -> true
    end.

