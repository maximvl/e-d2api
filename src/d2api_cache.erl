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
         get_leagues/0,
         get_live/0,
         get_schedule/1]).

get_team(Id) ->
  gen_cache:get_object(team_cache, Id).

get_match(Id) ->
  gen_cache:get_object(match_cache, Id).

get_leagues() ->
  gen_cache:get_object(league_cache, none).

get_live() ->
  gen_cache:get_object(live_cache, none).

get_schedule(Id) ->
  gen_cache:get_object(schedule_cache, Id).

get_player(Id) ->
  gen_cache:get_object(player_cache, Id).

%%%===================================================================
%%% Internal functions
%%%===================================================================
