%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 29 May 2013 by  <>
%%%-------------------------------------------------------------------
-module(team_cache).

-behaviour(gen_cache).

-export([init/0, fetch/2, verify_id/1]).

-define(EXPIRE, {1,{0,0,0}}).

init() ->
  Key = application:get_env(api_key_team),
  {ok, Key}.

verify_id({Start, Num}) when 
    is_integer(Id)  andalso Id > 0 andalso
    is_integer(Num) andalso Num > 0 ->
  ok;

verify_id(_) ->
  badarg.

fetch(Id, Key) ->
  Link = gen_link(Id, Key),
  {ok, {200, Body}} = httpc:request(get, Link, [], [{body_format, binary},
                                                    {full_request, false}]),
  Team = mochijson2:decode(Body),
  {ok, Team, ?EXPIRE}.

gen_link(Id, Key) ->
  "http://api.steampowered.com/IDOTA2Match_570/GetTeamInfoByTeamID/v1?key=" ++
    Key ++ "&start_at_team_id=" ++ Id ++ "&teams_requested=1".
