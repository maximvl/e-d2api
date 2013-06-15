-module(d2api_stats).

-export([start/0, update_stat/1, team_stats/0,
         date_stats/1, date_stats/2]).

-include("d2api.hrl").

start() ->
  ets:new(?STAT_ETS, [named_table, set, public]).

update_stat(Mod) ->
  Key = {Mod, date()},
  case ets:insert_new(?STAT_ETS, {Key, 1}) of
    true ->
      ok;
    _ ->
      ets:update_counter(?STAT_ETS, Key, {2, 1})
  end.

team_stats() ->
  ets:match_object(?STAT_ETS, {{?TEAMS_CACHE, '_'}, '_'}).

date_stats(Date) ->
  ets:match_object(?STAT_ETS, {{'_', Date}, '_'}).

date_stats({Y,M,D}, {Y2,M2,D2}) ->
  ets:select(?STAT_ETS, [{
                         [{{'_', {'$1','$2','$3'}}, '_'}],
                         [{'andalso',
                           {'>=', '$1', Y},
                           {'=<', '$1', Y2},
                           {'>=', '$2', M},
                           {'=<', '$2', M2},
                           {'>=', '$3', D},
                           {'<=', '$3', D2}}
                         ],
                         ['$_']}]).
