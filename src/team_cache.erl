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

-export([start_link/0]).
-export([init/0, fetch/2, verify_id/1]).

-define(EXPIRE, {1,{0,0,0}}).

-include("d2api.hrl").

start_link() ->
  gen_cache:start_link(?MODULE).

init() ->
  {ok, Key} = d2api:get_api_key(?TEAM_API_KEY),
  {ok, Key}.

verify_id({Start, Num}) when 
    is_integer(Start)  andalso Start > 0 andalso
    is_integer(Num) andalso Num > 0 ->
  ok;

verify_id(_) ->
  badarg.

fetch(Id, Key) ->
  Link = gen_link(Id, Key),

  error_logger:info_report("fetching: " ++ Link),
  {ok, {Status, Body}} = httpc:request(get, {Link, []},
                                       [{timeout, 5000}],
                                       [{body_format, binary},
                                        {full_result, false}]),
  error_logger:info_report("done"),
  case Status of
    200 ->
      {ok, Body, ?EXPIRE};
    _ ->
      {http_status, Status}
  end.

gen_link({Start, Num}, Key) ->
  StartStr = integer_to_list(Start),
  NumStr = integer_to_list(Num),
  "http://api.steampowered.com/IDOTA2Match_570/GetTeamInfoByTeamID/v1?key=" ++
    Key ++ "&start_at_team_id=" ++ StartStr ++ "&teams_requested=" ++ NumStr.
