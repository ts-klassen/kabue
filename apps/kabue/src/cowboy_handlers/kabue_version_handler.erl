-module(kabue_version_handler).
-behaviour(cowboy_rest).

-export([
        init/2
      , allowed_methods/2
      , content_types_provided/2
      , to_text/2
    ]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    { [<<"GET">>], Req, State }.

content_types_provided(Req, State) ->
    {[{<<"text/plain">>, to_text}], Req, State}.

to_text(Req, State) ->
    {ok, Version} = application:get_env(kabue, version),
    {Version, Req, State}.

