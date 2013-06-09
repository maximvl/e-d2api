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

-export([start_link/0]).
-export([init/0, verify_id/1, fetch/2]).

-define(EXPIRE, {0,{0,1,0}}).

-include("d2api.hrl").

start_link() ->
  gen_cache:start_link(?MODULE).

init() ->
  {ok, Key} = d2api:get_api_key(?MATCH_API_KEY),
  {ok, Key}.

verify_id(Id) when is_integer(Id) andalso Id > 0 ->
  ok;

verify_id(_) ->
  badarg.

fetch(Id, Key) ->
  Link = gen_link(Id, Key),
  {ok, {Status, Body}} = httpc:request(get, Link, [], [{body_format, binary},
                                                       {full_request, false}]),
  case Status of
    200 ->
      {ok, Body, ?EXPIRE};
    _ ->
      {http_status, Status}
  end.

gen_link(Id, Key) ->
  IdStr = integer_to_list(Id),
  "http://api.steampowered.com/IDOTA2Match_570/GetMatchDetails/v1?key=" ++
    Key ++ "&match_id=" ++ IdStr.
