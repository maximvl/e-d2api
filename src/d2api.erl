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

-export([get_player/1,
         get_team/1,
         get_match/1,
         get_leagues/0,
         get_live/0,
         get_schedule/1]).

set_api_key(Key) ->
  [application:set_env(d2api, X, Key) ||
    X <- [?TEAMS_KEY, ?MATCH_KEY,
          ?LEAGUES_KEY, ?LIVE_GAMES_KEY,
          ?SCHEDULED_GAMES_KEY]].

get_api_key(Unit) ->
  application:get_env(d2api, Unit).

get_team(Id) ->
  gen_cache:get_object(?TEAMS_CACHE, Id).

get_match(Id) ->
  gen_cache:get_object(?MATCH_CACHE, Id).

get_leagues() ->
  gen_cache:get_object(?LEAGUES_CACHE, none).

get_live() ->
  gen_cache:get_object(?LIVE_GAMES_CACHE, none).

get_schedule(Id) ->
  gen_cache:get_object(?SCHEDULED_GAMES_CACHE, Id).

get_player(Id) ->
  gen_cache:get_object(player, Id).

%%%===================================================================
%%% Internal functions
%%%===================================================================
