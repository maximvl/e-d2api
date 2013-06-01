%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 29 May 2013 by  <>
%%%-------------------------------------------------------------------
-module(schedule_cache).

-behaviour(gen_cache).

-export([init/0, verify_id/1, fetch/2]).

-define(EXPIRE, {0,{0,30,0}}).

init() ->
  Key = application:get_env(api_key_schedule),
  {ok, Key}.

verify_id({Min, Max}) when
    is_integer(Min) andalso Min > 0 andalso
    is_integer(Max) andalso Max > 0 ->
  ok;

verify_id(_) ->
  badarg.

fetch(Id, Key) ->
  Link = gen_link(Id, Key),
  {ok, {200, Body}} = httpc:request(get, Link, [], [{body_format, binary},
                                                    {full_request, false}]),
  Games = mochijson2:decode(Body),
  {ok, Games, ?EXPIRE}.

gen_link({MinDate, MaxDate}, Key) ->
  "http://api.steampowered.com/IDOTA2Match_570/GetScheduledLeagueGames/v1?key=" ++
    Key ++ "&date_min=" ++ MinDate ++ "&date_max=" ++ MaxDate. 
