%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 29 May 2013 by  <>
%%%-------------------------------------------------------------------
-module(gen_cache).

-behaviour(gen_server).

-callback init() -> {ok, term()}.
-callback fetch(integer(), term()) -> {term(), daystime()}.

%% API
-export([start_link/1, get_item/2, process_fetch/5]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {mod :: atom(),
                mod_state :: atom(),
                in_progress = dict:new() :: dict()}).

-define(SECS_IN_DAY, 86400).
-define(DONE_MSG(X), {retry, X}).

-type daystime() :: {integer(), calendar:time()}.

-record(cache_obj, {id :: integer(),
                    value :: term(),
                    expire :: calendar:datetime()
                   }).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Mod) ->
  gen_server:start_link({local, Mod}, ?MODULE, [Mod], []).

get_item(Mod, Id) ->
  Ets = Mod,
  case ets:lookup(Ets, Id) of
    [C] ->
      case is_expired(C) of
        false ->
          C;
        _ ->
          fetch_item(Mod, Ets, Id)
      end;
    [] ->
      fetch_item(Mod, Ets, Id)
  end.

fetch_item(Mod, Ets, Id) ->
  gen_server:call(Mod, {fetch, Id}),
  receive
    {retry, Id} ->
      [C] = ets:lookup(Ets, Id),
      C
  end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Mod]) ->
  {ok, MState} = Mod:init(),
  ets:new(Mod, [set, named,
                {keypos, #cache_obj.id},
                {read_concurrency, true}]),
  {ok, #state{mod = Mod,
              mod_state = MState}}.

handle_call({fetch, Id}, {ReplyTo, _}, State) ->
  InProgress = State#state.in_progress,
  NewDict = case dict:is_key(Id, InProgress) of
              true ->
                dict:append(Id, ReplyTo);
              false ->
                Mod = State#state.mod,
                Ets = Mod,
                MState = State#state.mod_state,
                spawn(?MODULE, process_fetch, [Mod, Ets, Id, ReplyTo, MState]),
                dict:store(Id, [ReplyTo], InProgress)
            end,
  {reply, wait, State#state{in_progress = NewDict}};

handle_call(_Req, _From, State) ->
  {reply, ok, State}.

handle_cast({fetched, Id}, State) ->
  InProgress = State#state.in_progress,
  Waiters = dict:fetch(Id, InProgress),
  [W ! ?DONE_MSG(Id) || W <- Waiters],
  NewDict = dict:erase(Id, InProgress),
  {noreply, State#state{in_progress = NewDict}};

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

process_fetch(Mod, Ets, Id, _ReplyTo, MState) ->
  {Data, Expire} = Mod:fetch(Id, MState),
  ets:insert(Ets, new_cache_obj(Id, Data, Expire)),
  gen_server:cast(Mod, {fetched, Id}).

-spec new_cache_obj(integer(), term(), daystime()) -> #cache_obj{}.
new_cache_obj(Id, Obj, Expire) ->
  #cache_obj{id = Id,
             value = Obj,
             expire = expire_datetime(Expire)}.

-spec is_expired(calendar:datetime()) -> boolean().
is_expired(Obj) when is_record(Obj, cache_obj) ->
  Obj#cache_obj.expire > calendar:universal_time();

is_expired(_) ->
  false.

-spec daystime_to_seconds(daystime()) -> calendar:seconds().
daystime_to_seconds({Days, Time}) ->
  calendar:time_to_seconds(Time) + (Days * ?SECS_IN_DAY).

-spec expire_datetime(daystime()) -> calendar:datetime().
expire_datetime(LifeTime) ->
  T1 = daystime_to_seconds(LifeTime),
  T2 = calendar:datetime_to_gregorian_seconds(
         calendar:universal_time()),
  calendar:gregorian_seconds_to_datetime(T1 + T2).
