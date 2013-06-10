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

teams(Start, Num) ->
  gen_cache:get_object(?TEAMS_CACHE, {Start, Num}).

match(Id) ->
  gen_cache:get_object(?MATCH_CACHE, Id).

leagues() ->
  gen_cache:get_object(?LEAGUES_CACHE, none).

live_games() ->
  gen_cache:get_object(?LIVE_GAMES_CACHE, none).

scheduled_games(From, To) ->
  gen_cache:get_object(?SCHEDULED_GAMES_CACHE, {From, To}).

player(Id) ->
  gen_cache:get_object(player, Id).

%%%===================================================================
%%% Internal functions
%%%===================================================================
