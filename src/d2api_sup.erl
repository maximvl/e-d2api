-module(d2api_sup).

-behaviour(supervisor).

-include("d2api.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Name, Fun),
        {Name, {I, start_link, [Name, Fun]}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  Caches = [{?TEAMS_CACHE,           fun api_link:teams/1},
            {?MATCH_CACHE,           fun api_link:match/1},
            {?LEAGUES_CACHE,         fun api_link:leagues/1},
            {?LIVE_GAMES_CACHE,      fun api_link:live_games/1},
            {?SCHEDULED_GAMES_CACHE, fun api_link:scheduled_games/1}],
  
  Children = [?CHILD(gen_cache, worker, Name, Fun) || {Name, Fun} <- Caches],
  d2api_stats:start(),
  {ok, { {one_for_one, 5, 10}, Children} }.
