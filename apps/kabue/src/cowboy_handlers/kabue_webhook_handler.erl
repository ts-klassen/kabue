-module(kabue_webhook_handler).
-behaviour(cowboy_rest).

-export([
        init/2
      , allowed_methods/2
      , resource_exists/2
      , content_types_accepted/2
      , webhook/2
    ]).

init(Req, State) ->
    %% Cache webhook configuration.
    Conf = case application:get_env(kabue, webhook) of
        {ok, Val} -> Val;
        _ -> []
    end,
    State1 = maps:put(webhook_conf, Conf, State),
    {cowboy_rest, Req, State1}.

allowed_methods(Req, State) ->
    { [<<"POST">>], Req, State }.

resource_exists(Req=#{bindings:=#{id:=Id}}, State=#{webhook_conf := Conf}) ->
    Exists = case proplists:lookup(Id, Conf) of
        none -> false;
        _ -> true
    end,
    {Exists, Req, State};
resource_exists(Req, State) ->
    {false, Req, State}.


content_types_accepted(Req, State) ->
    {[{'*', webhook}], Req, State}.


webhook(Req, State=#{webhook_conf := Conf}) ->
    {_, {Mod, Fun, 1}} = proplists:lookup(klsn_map:get([bindings, id], Req), Conf),
    {ok, ReqBody, Req1} = cowboy_req:read_body(Req),
    Req2 = cowboy_req:set_resp_body(Mod:Fun(ReqBody), Req1),
    {true, Req2, State}.

