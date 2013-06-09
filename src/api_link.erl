-module(api_link).

-include("d2api.hrl").

-export([teams/1, match/1, leagues/1, live_games/1, scheduled_games/1]).

-define(PREFIX, "http://api.steampowered.com/IDOTA2Match_570").

-spec teams({string(), string()}) -> {ok, string(), gen_cache:daystime()} | badarg.
teams({Start, Num}) when
    is_list(Start) andalso is_list(Num) ->
  {ok, Key} = application:get_env(?TEAMS_KEY),
  {ok, Expire} = application:get_env(?TEAMS_EXPIRE),
  {ok,
   ?PREFIX ++ "/GetTeamInfoByTeamID/v1?key=" ++ Key ++ "&start_at_team_id=" ++
     Start ++ "&teams_requested=" ++ Num,
   Expire};

teams(_) ->
  badarg.

-spec match(string()) -> {ok, string(), gen_cache:daystime()} | badarg.
match(Id) when is_list(Id) ->
  {ok, Key} = application:get_env(?MATCH_KEY),
  {ok, Expire} = application:get_env(?MATCH_EXPIRE),
  {ok,
   ?PREFIX ++ "/GetMatchDetails/v1?key=" ++ Key ++ "&match_id=" ++ Id,
   Expire};

match(_) ->
  badarg.

-spec leagues(any()) -> {ok, string(), gen_cache:daystime()}.
leagues(_) ->
  {ok, Key} = application:get_env(?LEAGUES_KEY),
  {ok, Expire} = application:get_env(?LEAGUES_EXPIRE),
  {ok,
   ?PREFIX ++ "/GetLeagueListing/v1?language=ru&key=" ++ Key,
   Expire}.

-spec live_games(any()) -> {ok, string(), gen_cache:daystime()}.
live_games(_) ->
  {ok, Key} = application:get_env(?LIVE_GAMES_KEY),
  {ok, Expire} = application:get_env(?LIVE_GAMES_EXPIRE),
  {ok,
   ?PREFIX ++ "/GetLiveLeagueGames/v1?key=" ++ Key,
   Expire}.

-spec scheduled_games({string(), string()}) -> {ok, string(), gen_cache:daystime()} | badarg.
scheduled_games({MinDate, MaxDate}) when
    is_list(MinDate) andalso is_list(MaxDate) ->
  {ok, Key} = application:get_env(?SCHEDULED_GAMES_KEY),
  {ok, Expire} = application:get_env(?SCHEDULED_GAMES_EXPIRE),
  {ok,
   ?PREFIX ++ "/GetScheduledLeagueGames/v1?key=" ++ Key ++ "&date_min=" ++
     MinDate ++ "&date_max=" ++ MaxDate,
   Expire};

scheduled_games(_) ->
  badarg.
