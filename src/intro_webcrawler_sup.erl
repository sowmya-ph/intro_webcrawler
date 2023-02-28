%%%-------------------------------------------------------------------
%% @doc intro_webcrawler top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(intro_webcrawler_sup).

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Nodes) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Nodes).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init(Nodes) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{id => intro_webcrawler_db_service_worker,
                  start => {intro_webcrawler_db_service_worker, start_link, [Nodes]},
                  restart => permanent,
                  shutdown => brutal_kill,
                  type => worker,
                  modules => [intro_webcrawler_db_service_worker]}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
