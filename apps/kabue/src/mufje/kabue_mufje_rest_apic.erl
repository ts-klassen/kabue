-module(kabue_mufje_rest_apic).

-export_type([
        options/0
      , request/0
      , payload/0
      , response_left/0
      , either/1
      , symbol/0
      , order_id/0
      , ticker/0
    ]).

-export([
        request/2
      , token/1
      , ranking/2
      , order/2
      , register/2
      , unregister/2
      , unregister_all/1
      , symbol/2
    ]).


-type options() :: #{
        uri_map := uri_string:uri_map()
      , mode := real | test
      , password => fun(() -> klsn:binstr())
      , token => klsn:binstr()
      , on_token_auto_update => fun((klsn:binstr()) -> any())
      , retry_left := non_neg_integer()
      , retry_sleep => non_neg_integer() % milliseconds
      , last_left => response_left()
      , print_left_info_msg => true
    }.

-type request() :: #{
        method := get | post | put
      , uri := klsn:binstr()
      , payload => payload()
      , q => #{}
    }.

-type payload() :: #{}.

-type response_left() :: #{
        resp := term()
      , status => 200..599
      , payload => #{}
      % https://kabucom.github.io/kabusapi/ptal/error.html
      , code => integer()
      , too_many_retry => boolean()
    }.

-type either(A) :: {right, A} | {left, response_left()}.

-type symbol() :: klsn:binstr().

-type order_id() :: klsn:binstr().

-type ticker() :: #{
        symbol => symbol()
      , exchange => kabue_mufje_enum:exchange()
    }.


-spec ranking(
        #{
            type => kabue_mufje_enum:ranking_type()
          , exchange_division => kabue_mufje_enum:exchange_division()
        }
      , options()
    ) -> ok.
ranking(ReqPayload0, Options) ->
    ReqQuery= maps:from_list(lists:filtermap(fun
        ({type, Type}) ->
            Enum = kabue_mufje_enum:ranking_type(),
            {true, {<<"Type">>, maps:get(Type, Enum)}};
        ({exchange_division, ExchangeDivision}) ->
            Enum = kabue_mufje_enum:exchange_division(),
            {true, {<<"ExchangeDivision">>, maps:get(ExchangeDivision, Enum)}};
        (_) -> false
    end, maps:to_list(ReqPayload0))),
    request(#{
        uri => <<"/kabusapi/ranking">>
      , method => get
      , q => ReqQuery
    } , Options).


-spec order(
        #{
            symbol := symbol()
          , exchange := kabue_mufje_enum:exchange()
          , security_type := kabue_mufje_enum:security_type()
          , side := kabue_mufje_enum:side()
          , cash_margin := kabue_mufje_enum:cash_margin()
          , margin_trade_type => kabue_mufje_enum:margin_trade_type()
          , margin_premium_unit => float()
          , deliv_type := kabue_mufje_enum:deliv_type()
          , fund_type => kabue_mufje_enum:fund_type()
          , account_type := kabue_mufje_enum:account_type()
          , qty := integer()
          , close_position_order => kabue_mufje_enum:close_position_order()
          , close_positions => [#{
                hold_id := klsn:binstr()
              , qty := integer()
            }]
          , front_order_type := kabue_mufje_enum:front_order_type()
          , price := float
          , expire_day := integer()
          , reverse_limit_order => #{
                trigger_sec := kabue_mufje_enum:trigger_sec()
              , trigger_price := float()
              , under_over := kabue_mufje_enum:under_over()
              , after_hit_order_type := kabue_mufje_enum:after_hit_order_type()
              , after_hit_price := float()
            }
        }
      , options()
    ) -> either(order_id()).
order(ReqPayload0, Options) ->
    ReqPayload = maps:from_list(lists:filtermap(fun
        ({symbol, Symbol}) ->
            {true, {<<"Symbol">>, Symbol}};
        ({exchange, Exchange}) ->
            Enum = kabue_mufje_enum:exchange(),
            {true, {<<"Exchange">>, maps:get(Exchange, Enum)}};
        ({security_type, SecurityType}) ->
            Enum = kabue_mufje_enum:security_type(),
            {true, {<<"SecurityType">>, maps:get(SecurityType, Enum)}};
        ({side, Side}) ->
            Enum = kabue_mufje_enum:side(),
            {true, {<<"Side">>, maps:get(Side, Enum)}};
        ({cash_margin, CashMargin}) ->
            Enum = kabue_mufje_enum:cash_margin(),
            {true, {<<"CashMargin">>, maps:get(CashMargin, Enum)}};
        ({margin_trade_type, MarginTradeType}) ->
            Enum = kabue_mufje_enum:margin_trade_type(),
            {true, {<<"MarginTradeType">>, maps:get(MarginTradeType, Enum)}};
        ({margin_premium_unit, MarginPremiumUnit}) ->
            {true, {<<"MarginPremiumUnit">>, MarginPremiumUnit}};
        ({deliv_type, DelivType}) ->
            Enum = kabue_mufje_enum:deliv_type(),
            {true, {<<"DelivType">>, maps:get(DelivType, Enum)}};
        ({fund_type, FundType}) ->
            Enum = kabue_mufje_enum:fund_type(),
            {true, {<<"FundType">>, maps:get(FundType, Enum)}};
        ({account_type, AccountType}) ->
            Enum = kabue_mufje_enum:account_type(),
            {true, {<<"AccountType">>, maps:get(AccountType, Enum)}};
        ({qty, Qty}) ->
            {true, {<<"Qty">>, Qty}};
        ({close_position_order, ClosePositionOrder}) ->
            Enum = kabue_mufje_enum:close_position_order(),
            Value = maps:get(ClosePositionOrder, Enum),
            {true, {<<"ClosePositionOrder">>, Value}};
        ({close_positions, ClosePositions}) ->
            lists:map(fun(#{hold_id:=HoldID, qty:=Qty})->
                #{<<"HoldID">> => HoldID, <<"Qty">> => Qty}
            end, ClosePositions);
        ({front_order_type, FrontOrderType}) ->
            Enum = kabue_mufje_enum:front_order_type(),
            {true, {<<"FrontOrderType">>,  maps:get(FrontOrderType, Enum)}};
        ({price, Price}) ->
            {true, {<<"Price">>, Price}};
        ({expire_day, ExpireDay}) ->
            {true, {<<"ExpireDay">>, ExpireDay}};
        ({reverse_limit_order, ReverseLimitOrder}) ->
            #{
                <<"TriggerSec">> =>
                    maps:get(
                        maps:get(trigger_sec, ReverseLimitOrder)
                      , kabue_mufje_enum:trigger_sec()
                    )
              , <<"TriggerPrice">> =>
                    maps:get(trigger_price, ReverseLimitOrder)
              , <<"UnderOver">> =>
                    maps:get(
                        maps:get(under_over, ReverseLimitOrder)
                      , kabue_mufje_enum:under_over()
                    )
              , <<"AfterHitOrderType">> =>
                    maps:get(
                        maps:get(after_hit_order_type, ReverseLimitOrder)
                      , kabue_mufje_enum:after_hit_order_type()
                    )
              , <<"AfterHitPrice">> =>
                    maps:get(after_hit_price, ReverseLimitOrder)
            };
        (_) -> false
    end, maps:to_list(ReqPayload0))),
    Res = request(#{
        uri => <<"/kabusapi/sendorder">>
      , method => post
      , payload => ReqPayload
    }, Options),
    case Res of
        {right, #{<<"OrderId">> := Id, <<"Result">> := 0}} ->
            {right, Id};
        {left, Left} ->
            {left, Left}
    end.


%% ------------------------------------------------------------------
%%  Symbol information
%% ------------------------------------------------------------------

-spec symbol(ticker(), options()) -> either(payload()).
symbol(#{symbol := SymbolBin, exchange := ExchangeAtom}, Options) ->
    ExchangeCode = maps:get(ExchangeAtom, kabue_mufje_enum:exchange()),
    Uri = iolist_to_binary([
        "/kabusapi/symbol/",
        SymbolBin,
        "@",
        klsn_binstr:from_any(ExchangeCode)
    ]),
    request(#{uri => Uri, method => get}, Options).


-spec register(
        [#{
            symbol => symbol()
          , exchange => kabue_mufje_enum:exchange()
        }]
      , options()
    ) -> either(ticker()).
register(ReqPayload0, Options) ->
    Symbols = lists:map(fun(Elem) ->
        #{
            <<"Symbol">> => maps:get(symbol, Elem)
          , <<"Exchange">> => maps:get(maps:get(exchange, Elem), kabue_mufje_enum:exchange())
        }
    end, ReqPayload0),
    parse_regist_list(request(#{
        uri => <<"/kabusapi/register">>
      , method => put
      , payload => #{ <<"Symbols">> => Symbols }
    } , Options)).


-spec unregister(
        [#{
            symbol => symbol()
          , exchange => kabue_mufje_enum:exchange()
        }]
      , options()
    ) -> either(ticker()).
unregister(ReqPayload0, Options) ->
    Symbols = lists:map(fun(Elem) ->
        #{
            <<"Symbol">> => maps:get(symbol, Elem)
          , <<"Exchange">> => maps:get(maps:get(exchange, Elem), kabue_mufje_enum:exchange())
        }
    end, ReqPayload0),
    parse_regist_list(request(#{
        uri => <<"/kabusapi/unregister">>
      , method => put
      , payload => #{ <<"Symbols">> => Symbols }
    } , Options)).


-spec unregister_all(options()) -> either(ticker()).
unregister_all(Options) ->
    parse_regist_list(request(#{
        uri => <<"/kabusapi/unregister/all">>
      , method => put
    } , Options)).


-spec parse_regist_list(either(payload())) -> either(ticker()).
parse_regist_list(Payload) ->
    case Payload of
        {right, #{<<"RegistList">>:=RegistList}} ->
            Enum = klsn_map:invert(kabue_mufje_enum:exchange()),
            Res = lists:map(fun
                (#{<<"Exchange">>:=Exchange, <<"Symbol">>:=Symbol})->
                    #{
                        symbol => Symbol
                      , exchange => maps:get(Exchange, Enum)
                    }
            end, RegistList),
            {right, Res};
        {left, Left} ->
            {left, Left}
    end.


-spec token(options()) -> either(klsn:binstr()).
token(Options0) ->
    Options = options(Options0),
    Password = case Options of
        #{ password := PasswdFun } ->
            PasswdFun();
        _ ->
            Env = case Options of
                #{mode := real} ->
                    "KABUE_KABUCOM_PASSWORD";
                #{mode := test} ->
                    "KABUE_KABUCOM_TEST_PASSWORD"
            end,
            case os:getenv(Env) of
                false ->
                    <<>>;
                Str when is_list(Str) ->
                    iolist_to_binary(Str)
            end
    end,
    Res = request(#{
        uri => <<"/kabusapi/token">>
      , method => post
      , payload => #{ <<"APIPassword">> => Password }
    }, Options),
    case Res of
        {right, #{<<"ResultCode">> := 0, <<"Token">> := Token}} ->
            {right, Token};
        {left, Left} ->
            {left, Left}
    end.


-spec options() -> options().
options() ->
    #{
      % uri_map => #{
      %     scheme => <<"http">>
      %   , host => <<"localhost">>
      %   , port => 18080
      % }
        mode => test
      , retry_left => 3
      , retry_sleep => 0
    }.


-spec options(options()) -> options().
options(Options0) ->
    Options = maps:merge(options(), Options0),
    case Options of
        #{uri_map := _} ->
            Options;
        #{mode := real} ->
            Options#{
                uri_map => #{
                    scheme => <<"http">>
                  , host => <<"localhost">>
                  , port => 18080
                }
            };
        #{mode := test} ->
            Options#{
                uri_map => #{
                    scheme => <<"http">>
                  , host => <<"localhost">>
                  , port => 18081
                }
            }
    end.


-spec request(request(), options()) -> either(payload()).
request(Request, Options0) ->
    Options = options(Options0),
    try request_(Request, Options) of
        {right, Right} ->
            {right, Right};
        {left, Left=#{status:=401, payload:=#{<<"Code">>:=4001009}}} ->
            RetryOptions = Options#{
                last_left => Left
              , retry_left => maps:get(retry_left, Options) - 1
            },
            case token(RetryOptions) of
                {right, Token} ->
                    case RetryOptions of
                        #{on_token_auto_update:=UpdateFun} ->
                            spawn(fun()->
                                UpdateFun(Token)
                            end);
                        _ ->
                            ok
                    end,
                    request(Request, RetryOptions#{
                        token => Token
                    });
                Other ->
                    Other
            end;
        Other ->
            Other
    catch
        throw:{?MODULE, too_many_retry, Return} ->
            Return
    end.


-spec request_(request(), options()) -> either(payload()).
request_(Request=#{method:=Method, uri:=Uri}, Options) ->
    case Options of
        #{retry_left := N, retry_sleep := Sleep} when N > 0 ->
            timer:sleep(Sleep);
        #{last_left := LastLeft} ->
            throw({?MODULE, too_many_retry, {left, LastLeft#{
                too_many_retry => true
            }}});
        _ ->
            error(too_many_retry)
    end,
    UriMap10 = klsn_map:get([uri_map], Options),
    UriMap20 = UriMap10#{ path => Uri },
    UriMap30 = case Request of
        #{ q := Query } ->
            QueryStr = uri_string:compose_query(lists:map(fun
                ({Key, Val}) ->
                    {Key, klsn_binstr:from_any(Val)};
                (Pair) -> Pair
            end, maps:to_list(Query))),
            UriMap20#{'query' => QueryStr};
        _ ->
            UriMap20
    end,
    Url = uri_string:recompose(UriMap30),
    ReqHeaders = case Options of
        #{token := Token} ->
            [{"X-API-KEY", Token}];
        _ ->
            []
    end,
    Req = case klsn_map:lookup([payload], Request) of
        none ->
            {Url, ReqHeaders};
        {value, JSON} when is_map(JSON) ->
            {Url, ReqHeaders, "application/json", jsone:encode(JSON)}
    end,
    HttpcRes = httpc:request(Method, Req, [], [{body_format, binary}]),
    case HttpcRes of
        {ok, {{_, Status, _}, _ResHeaders, ResBody}} when 200 =< Status, Status =< 299 ->
            {right, jsone:decode(ResBody)};
        {ok, {{_, Status, _}, _ResHeaders, ResBody}} ->
            Left0 = #{
                resp => HttpcRes
              , status => Status
            },
            Left10 = try jsone:decode(ResBody) of
                LeftPayload ->
                    Left0#{
                        payload => LeftPayload
                    }
            catch
                error:badarg ->
                    Left0
            end,
            Left = case klsn_map:lookup([payload, <<"Code">>], Left10) of
                {value, Code} ->
                    Left10#{
                        code => Code
                    };
                none ->
                    Left10
            end,
            case
                {
                    klsn_map:lookup([payload, <<"Code">>], Left10)
                  , klsn_map:lookup([payload, <<"Message">>], Left10)
                  , klsn_map:lookup([print_left_info_msg], Options)
                }
            of
                {{value, CodeMsg}, {value, Message}, {value, true}} ->
                    error_logger:info_msg("~p:~p/~p error: ~p~n~ts~n~p~n", [
                        ?MODULE
                      , ?FUNCTION_NAME
                      , ?FUNCTION_ARITY
                      , CodeMsg
                      , Message
                      , Request
                    ]);
                _ ->
                    ok
            end,
            {left, Left};
        _ ->
            {left, #{ resp => HttpcRes }}
    end.

% lists:map(fun(#{<<"Symbol">>:=Ticker})-> kabue_rakuten_rss_market:add(Ticker) end, maps:get(<<"Ranking">>, element(2, kabue_mufje_rest_apic:ranking(trading_volume_top, #{})))).


