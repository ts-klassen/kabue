%%%-------------------------------------------------------------------
%% @doc kabue public API
%% @end
%%%-------------------------------------------------------------------

-module(kabue_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, AllowedEndpoints} = application:get_env(kabue, allowed_endpoints),
    Endpoints = fun
        (status) ->
            {true, {"/kabue/status", kabue_status_handler, #{}}}
      ; (version) ->
            {true, {"/kabue/version", kabue_version_handler, #{}}}
      ; (health) ->
            {true, {"/kabue/health", kabue_health_handler, #{}}}
      ; (webhook) ->
            {true, {"/kabue/webhook/:id", kabue_webhook_handler, #{}}}
      ; (market) ->
            {true, {"/kabue/market/:market/:ticker", kabue_market_handler, #{}}}
      ; (static) ->
            {true, {"/kabue/static/[...]", cowboy_static, {priv_dir, kabue, "static"}}}
      ; (hello_world) ->
            {true, {"/kabue/hello-world", kabue_hello_world_handler, #{}}}
      ; (_) ->
            false
    end,
    Dispatch = cowboy_router:compile([
            {'_', lists:filtermap(Endpoints, AllowedEndpoints)}
    ]),
    {ok, Port} = application:get_env(kabue, port),
    {ok, _} = cowboy:start_clear(kabue_http_listener,
        [{port, Port}, inet, inet6],
        #{env => #{dispatch => Dispatch}
    }),
    kabue_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
