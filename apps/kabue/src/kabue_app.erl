%%%-------------------------------------------------------------------
%% @doc kabue public API
%% @end
%%%-------------------------------------------------------------------

-module(kabue_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    kabue_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
