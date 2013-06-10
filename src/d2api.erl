%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 30 May 2013 by  <>
%%%-------------------------------------------------------------------
-module(d2api).
-include("d2api.hrl").

-export([set_api_key/1,
         get_api_key/1]).

-export([player/1,
         teams/2,
         match/1,
         leagues/0,
         live_games/0,
         scheduled_games/2]).

set_api_key(Key) ->
  [application:set_env(d2api, X, Key) ||
    X <- [?TEAMS_KEY, ?MATCH_KEY,
          ?LEAGUES_KEY, ?LIVE_GAMES_KEY,
          ?SCHEDULED_GAMES_KEY]].

get_api_key(Unit) ->
  application:get_env(d2api, Unit).

-spec teams(string(), string()) -> {ok, binary()} | badarg.
teams(Start, Num) when
    is_list(Start) andalso is_list(Num) ->
  gen_cache:get_object(?TEAMS_CACHE, {Start, Num});

teams(_, _) ->
  badarg.

-spec match(string()) -> {ok, binary()} | badarg.
match(Id) when is_list(Id) ->
  gen_cache:get_object(?MATCH_CACHE, Id);

match(_) ->
  badarg.

-spec leagues() -> {ok, binary()}.
leagues() ->
  gen_cache:get_object(?LEAGUES_CACHE, none).

-spec live_games() -> {ok, binary()}.
live_games() ->
  gen_cache:get_object(?LIVE_GAMES_CACHE, none).

-spec scheduled_games(string(), string()) -> {ok, binary()} | badarg.
scheduled_games(From, To) when
    is_list(From) andalso is_list(To) ->
  gen_cache:get_object(?SCHEDULED_GAMES_CACHE, {From, To});

scheduled_games(_, _) ->
  badarg.

player(Id) ->
  gen_cache:get_object(player, Id).

%%%===================================================================
%%% Internal functions
%%%===================================================================
