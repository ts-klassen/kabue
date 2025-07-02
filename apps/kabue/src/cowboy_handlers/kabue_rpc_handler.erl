-module(kabue_rpc_handler).

-behaviour(cowboy_rest).

-export([
        init/2
      , allowed_methods/2
      , resource_exists/2
      , content_types_accepted/2
      , rpc/2
      , allow_missing_post/2
    ]).

%%----------------------------------------------------------------------
%% cowboy_rest callbacks
%%----------------------------------------------------------------------

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    { [<<"POST">>], Req, State }.

%% Verify the rpc function exists. If not, return 404.
resource_exists(Req=#{bindings := #{id := IdBin}}, State) ->
    Exists = case try_binary_to_existing_atom(IdBin) of
        {ok, IdAtom} ->
            erlang:function_exported(kabue_rpc, IdAtom, 1);
        error ->
            false
    end,
    {Exists, Req, State};
resource_exists(Req, State) ->
    {false, Req, State}.

%% Accept any incoming content-type so clients are not forced to set one.
%% We still expect the payload to be JSON and will attempt to decode it.
content_types_accepted(Req, State) ->
    {[{'*', rpc}], Req, State}.

%%----------------------------------------------------------------------
%% POST to a *missing* RPC should not be allowed.
%% The default (no callback) for allow_missing_post/2 is 'true', which would
%% make Cowboy continue processing the request even when resource_exists/2
%% returned false, eventually calling rpc/2 and crashing. We override it to
%% 'false' so Cowboy responds 404 immediately.
%%----------------------------------------------------------------------

allow_missing_post(Req, State) ->
    {false, Req, State}.


%%----------------------------------------------------------------------
%% Internal helpers
%%----------------------------------------------------------------------

try_binary_to_existing_atom(Bin) when is_binary(Bin) ->
    try {ok, binary_to_existing_atom(Bin, utf8)}
    catch
        error:badarg -> error
    end.

%%----------------------------------------------------------------------
%% Main logic
%%----------------------------------------------------------------------

rpc(Req=#{bindings := #{id := IdBin}}, State) ->
    IdAtomResult = try_binary_to_existing_atom(IdBin),
    case IdAtomResult of
        {ok, IdAtom} ->
            {ok, Body, Req1} = cowboy_req:read_body(Req),
            RequestMap = jsone:decode(Body),
            Response = apply(kabue_rpc, IdAtom, [RequestMap]),
            RespJson = jsone:encode(Response),
            Req2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req1),
            Req3 = cowboy_req:set_resp_body(RespJson, Req2),
            {true, Req3, State};
        error ->
            %% Should not happen because resource_exists should have returned false,
            %% but keep a guard in place.
            {false, Req, State}
    end.
