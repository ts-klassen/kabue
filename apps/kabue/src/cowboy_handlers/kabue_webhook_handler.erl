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
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    { [<<"POST">>], Req, State }.

resource_exists(Req=#{bindings:=#{id:=Id}}, State) ->
    Exists = case application:get_env(kabue, webhook) of
        {ok, PList} ->
            case proplists:lookup(Id, PList) of
                none ->
                    false;
                _ ->
                    true
            end;
        _ ->
            false
    end,
    {Exists, Req, State};
resource_exists(Req, State) ->
    {false, Req, State}.


content_types_accepted(Req, State) ->
    {[{'*', webhook}], Req, State}.


webhook(Req, State) ->
    {ok, Conf} = application:get_env(kabue, webhook),
    {_, {Mod, Fun, 1}} = proplists:lookup(klsn_map:get([bindings, id], Req), Conf),
    {ok, ReqBody, Req1} = cowboy_req:read_body(Req),
    Req2 = cowboy_req:set_resp_body(Mod:Fun(ReqBody), Req1),
    {true, Req2, State}.

