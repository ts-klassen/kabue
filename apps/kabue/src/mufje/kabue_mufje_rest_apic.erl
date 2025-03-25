-module(kabue_mufje_rest_apic).

-export_type([
        options/0
      , request/0
      , payload/0
      , response_left/0
      , response/0
    ]).

-export([
        request/2
      , token/1
      , ranking/2
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
      , too_many_retry => boolean()
    }.

-type response() :: {right, payload()} | {left, response_left()}.


-spec ranking(
        kabue_mufje_enum:ranking_type()
      , options()
    ) -> ok.
ranking(Type, Options) ->
    request(#{
        uri => <<"/kabusapi/ranking">>
      , method => get
      , q => #{<<"type">> => maps:get(Type, kabue_mufje_enum:ranking_type())}
    } , Options).

-spec token(options()) -> {right, klsn:binstr()} | {left, response_left()}.
token(Options) ->
    Password = case Options of
        #{ password := PasswdFun } ->
            PasswdFun();
        _ ->
            case os:getenv("KABUE_KABUCOM_PASSWORD") of
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
        uri_map => #{
            scheme => <<"http">>
          , host => <<"localhost">>
          , port => 18080
        }
      , mode => real
      , retry_left => 3
      , retry_sleep => 0
    }.


-spec options(options()) -> options().
options(Options) ->
    maps:merge(options(), Options).


-spec request(request(), options()) -> response().
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


-spec request_(request(), options()) -> response().
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
    Req = case Method of
        get ->
            {Url, ReqHeaders};
        post ->
            Payload = case klsn_map:get([payload], Request, <<>>) of
                JSON when is_map(JSON) ->
                    jsone:encode(JSON);
                Bin when is_binary(Bin) ->
                    Bin
            end,
            {Url, ReqHeaders, "application/json", Payload}
    end,
    HttpcRes = httpc:request(Method, Req, [], [{body_format, binary}]),
    case HttpcRes of
        {ok, {{_, Status, _}, _ResHeaders, ResBody}} when 200 =< Status, Status =< 299 ->
            {right, jsone:decode(ResBody)};
        {ok, {{_, Status, _}, _ResHeaders, ResBody}} ->
            {left, #{
                resp => HttpcRes
              , status => Status
              , payload => jsone:decode(ResBody)
            }};
        _ ->
            {left, #{ resp => HttpcRes}}
    end.


% lists:map(fun(#{<<"Symbol">>:=Ticker})-> kabue_rakuten_rss_market:add(Ticker) end, maps:get(<<"Ranking">>, element(2, kabue_mufje_rest_apic:ranking(trading_volume_top, #{})))).


