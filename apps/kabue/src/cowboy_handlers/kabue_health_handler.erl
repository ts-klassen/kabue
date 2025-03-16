-module(kabue_health_handler).
-behaviour(cowboy_rest).

-export([
        init/2
      , allowed_methods/2
      , service_available/2
      , content_types_provided/2
      , to_text/2
    ]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    { [<<"GET">>], Req, State }.

service_available(Req, State) ->
    {is_healthy(), Req, State}.

content_types_provided(Req, State) ->
    {[{<<"text/plain">>, to_text}], Req, State}.

to_text(Req, State) ->
    {<<"1">>, Req, State}.

is_healthy() ->
    kabue_status:health().

