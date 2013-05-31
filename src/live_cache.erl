%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 29 May 2013 by  <>
%%%-------------------------------------------------------------------
-module(live_cache).

-behaviour(gen_cache).

-export([init/0, fetch/2]).

-define(EXPIRE, {0,{0,30,0}}).

init() ->
  Key = application:get_env(api_key_live),
  {ok, Key}.

fetch(Id, Key) ->
  Link = gen_link(Id, Key),
  {ok, {200, Body}} = httpc:request(get, Link, [], [{body_format, binary},
                                                    {full_request, false}]),
  Games = mochijson2:decode(Body),
  {Games, ?EXPIRE}.

gen_link(_Id, Key) ->
  "http://api.steampowered.com/IDOTA2Match_570/GetLiveLeagueGames/v1?key=" ++ Key.
