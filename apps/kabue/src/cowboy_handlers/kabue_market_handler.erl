-module(kabue_market_handler).
-behaviour(cowboy_rest).

-export([
        init/2
      , allowed_methods/2
      , content_types_provided/2
      , to_json/2
    ]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    { [<<"GET">>], Req, State }.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

to_json(Req=#{bindings:=#{market:=<<"rakuten-rss">>, ticker:=Ticker}}, State) ->
    Market = lists:map(fun(Info)->
        maps:map(fun
            (_, {value, Value}) when is_tuple(Value) ->
                tuple_to_list(Value);
            (_, {value, Value}) ->
                Value;
            (_, none) ->
                null;
            (timestamp, Timestamp) ->
                iolist_to_binary(calendar:system_time_to_rfc3339(
                Timestamp
              , [{unit, nanosecond}]))
        end, Info)
    end, kabue_rakuten_rss_market:historical(#{ ticker => Ticker })),
    JSON = jsone:encode(Market, [{float_format, [{decimals,16}, compact]}]),
    {JSON, Req, State}.

