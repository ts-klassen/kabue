-module(kabue_status_handler).
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

to_json(Req, State) ->
    {jsone:encode(kabue_status:status()), Req, State}.

