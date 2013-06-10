-module(api_link).

-include("d2api.hrl").

-export([teams/1, match/1, leagues/1, live_games/1, scheduled_games/1]).

-define(PREFIX, "http://api.steampowered.com/IDOTA2Match_570").

-spec teams({string(), string()}) -> {string(), gen_cache:daystime()}.
teams({Start, Num}) ->
  {ok, Key} = application:get_env(?TEAMS_KEY),
  {ok, Expire} = application:get_env(?TEAMS_EXPIRE),
  {?PREFIX ++ "/GetTeamInfoByTeamID/v1?key=" ++ Key ++ "&start_at_team_id=" ++
     Start ++ "&teams_requested=" ++ Num,
   Expire}.

-spec match(string()) -> {string(), gen_cache:daystime()}.
match(Id) ->
  {ok, Key} = application:get_env(?MATCH_KEY),
  {ok, Expire} = application:get_env(?MATCH_EXPIRE),
  {?PREFIX ++ "/GetMatchDetails/v1?key=" ++ Key ++ "&match_id=" ++ Id,
   Expire}.

-spec leagues(any()) -> {string(), gen_cache:daystime()}.
leagues(_) ->
  {ok, Key} = application:get_env(?LEAGUES_KEY),
  {ok, Expire} = application:get_env(?LEAGUES_EXPIRE),
  {?PREFIX ++ "/GetLeagueListing/v1?language=ru&key=" ++ Key,
   Expire}.

-spec live_games(any()) -> {string(), gen_cache:daystime()}.
live_games(_) ->
  {ok, Key} = application:get_env(?LIVE_GAMES_KEY),
  {ok, Expire} = application:get_env(?LIVE_GAMES_EXPIRE),
  {?PREFIX ++ "/GetLiveLeagueGames/v1?key=" ++ Key,
   Expire}.

-spec scheduled_games({string(), string()}) -> {string(), gen_cache:daystime()}.
scheduled_games({MinDate, MaxDate}) ->
  {ok, Key} = application:get_env(?SCHEDULED_GAMES_KEY),
  {ok, Expire} = application:get_env(?SCHEDULED_GAMES_EXPIRE),
  {?PREFIX ++ "/GetScheduledLeagueGames/v1?key=" ++ Key ++ "&date_min=" ++
     MinDate ++ "&date_max=" ++ MaxDate,
   Expire}.
