%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 30 May 2013 by  <>
%%%-------------------------------------------------------------------
-module(d2api_cache).

-behaviour(gen_server).

-include("d2api_util.hrl").

%% API
-export([start_link/0,
         get_player/1, get_team/1,
         get_match/1, get_league/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_team(Id) ->
  get(team_fetcher, Id).

get_match(Id) ->
  get(match_fetcher, Id).

get_league(Id) ->
  get(league_fetcher, Id).

get_player(Id) ->
  get(player_fetcher, Id).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, #state{}}.

handle_call({fetch, Fetcher, Id}, _From, State) ->
  case Fetcher:get(Id) of
    [Obj] ->
      Obj;
    _ ->
      gen_server:call(Fetcher, {fetch, Id})
  end;

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get(Fetcher, Id) ->
  case maybe_get(Fetcher, Id) of
    not_found ->
      gen_server:call(?SERVER, {fetch, Fetcher, Id});
    Obj ->
      Obj
  end.

maybe_get(Fetcher, Id) ->
  case Fetcher:get(Id) of
    [C] ->
      Time = C#cache_obj.timestamp,
      case d2api_util:is_expired(Time) of
        false ->
          C;
        _ ->
          not_found
      end;
    _ ->
      not_found
  end.
