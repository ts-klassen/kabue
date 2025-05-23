%%%-------------------------------------------------------------------
%% @doc kabue top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(kabue_rakuten_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    ChildSpecs = [
        #{
            id => kabue_rakuten_rss_market_1
          , start => {kabue_rakuten_rss_market, start_link, []}
          , restart => permanent
          , type => worker
        }
    ],
    SupFlags = #{strategy => one_for_one,
                 intensity => length(ChildSpecs) + 1,
                 period => 1},
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
