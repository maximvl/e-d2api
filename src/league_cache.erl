%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 29 May 2013 by  <>
%%%-------------------------------------------------------------------
-module(league_cache).

-behaviour(gen_cache).

-export([init/0, verify_id/1, fetch/2]).

-define(EXPIRE, {5,{0,0,0}}).

init() ->
  Key = application:get_env(api_key_league),
  {ok, Key}.

verify_id(_) ->
  ok.

fetch(_Id, Key) ->
  Link = gen_link(Key),
  {ok, {200, Body}} = httpc:request(get, Link, [], [{body_format, binary},
                                                    {full_request, false}]),
  Leagues = mochijson2:decode(Body),
  {ok, Leagues, ?EXPIRE}.

gen_link(Key) ->
  "http://api.steampowered.com/IDOTA2Match_570/GetLeagueListing/v1?key="
    ++ Key ++ "&language=ru".
