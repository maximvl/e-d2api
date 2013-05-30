%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 29 May 2013 by  <>
%%%-------------------------------------------------------------------
-module(team_fetcher).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(?EXPIRE_TIME, {{0,0,0},{1,0,0}}).                 %% 1 hour

-record(team, {id :: integer(),
               name :: binary(),
               tag :: binary(),
               time_created :: calendar:datetime(),
               rating :: integer(),
               logo_id :: integer(),
               logo_sponsor_id :: integer(),
               country_code :: binary(),
               url :: binary(),
               games_with_roster :: integer(),
               players_id :: [integer()],
               admin_id :: integer()
              }).

-record(cache_obj, {id :: integer(),
                    value :: #team{},
                    expire :: calendar:datetime()
                   }).
                    
-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec get(integer()) -> [#team{}].
get(Id) ->
  ets:lookup(?SERVER, Id).

cache(Team) when is_record(Team, team) ->
  gen_server:cast(?SERVER, {cache, Team});

cache(_) ->
  ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  ets:new(?SERVER, [set, named,
                    {keypos, #cache_obj.id},
                    {read_concurrency, true}]).
  {ok, #state{}}.

handle_call({fetch, Id}, _From, State) ->
  Reply = fetch_and_cache(Id),
  {reply, Reply, State}.

handle_call(_Req, _From, State) ->
  {reply, ok, State}.

handle_cast({cache, Team}, State) ->
  cache(Obj),
  {noreply, State};

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

fetch_and_cache(ID) ->
  {ok, {200, Body}} = httpc:request(get, "", [], [{body_format, binary},
                                                  {full_request, false}]),
  Team = #team{id = 
               
new_cache_obj(Team) ->
  #cache_obj{id = Team#team.id,
             value = Team,
             expire = calendar:universal_time() + 


cache(Obj) ->
  ets:insert(?SERVER, Obj).
