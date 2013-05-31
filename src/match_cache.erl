%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 29 May 2013 by  <>
%%%-------------------------------------------------------------------
-module(match_cache).

-behaviour(gen_cache).

-export([init/0, fetch/2]).

-define(EXPIRE, {0,{1,0,0}}).

init() ->
  Key = application:get_env(api_key_match),
  {ok, Key}.

fetch(Id, Key) ->
  Link = gen_link(Id, Key),
  {ok, {200, Body}} = httpc:request(get, Link, [], [{body_format, binary},
                                                    {full_request, false}]),
  Match = mochijson2:decode(Body),
  {Match, ?EXPIRE}.

gen_link(Id, Key) ->
  "http://api.steampowered.com/IDOTA2Match_570/GetMatchDetails/v1?key=" ++
    Key ++ "&match_id=" ++ Id.