%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 30 May 2013 by  <>
%%%-------------------------------------------------------------------
-module(d2api_cache).

-export([get_player/1,
         get_team/1,
         get_match/1,
         get_league/1,
         get_live/1,
         get_schedule/1]).

get_team(Id) ->
  gen_cache:get_item(team_cache, Id).

get_match(Id) ->
  gen_cache:get_item(match_cache, Id).

get_league(Id) ->
  gen_cache:get_item(league_cache, Id).

get_live(Id) ->
  gen_cache:get_item(live_cache, Id).

get_schedule(Id) ->
  gen_cache:get_item(schedule_cache, Id).

get_player(Id) ->
  gen_cache:get_item(player_cache, Id).

%%%===================================================================
%%% Internal functions
%%%===================================================================
