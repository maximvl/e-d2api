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
-callback verify_id(term()) -> ok | badarg.
-callback fetch(integer(), term()) -> {ok, term(), daystime()} | term().

%% API
-export([start_link/1, get_object/2, do_cache/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {mod :: atom(),
                mod_state :: atom(),
                in_progress = dict:new() :: dict()}).

-define(SECS_IN_DAY, 86400).

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

-spec get_object(atom(), term()) -> term() | {error, term()}.
get_object(Mod, Id) ->
  get_object(Mod, Id, Mod:verify_id(Id)).

-spec get_object(atom(), term(), ok | badarg) -> term() | {error, term()}.
get_object(Mod, Id, ok) ->
  Ets = Mod,
  case ets:lookup(Ets, Id) of
    [O] ->
      case is_expired(O) of
        false ->
          O#cache_obj.value;
        _ ->
          cache_object(Mod, Ets, Id)
      end;
    [] ->
      cache_object(Mod, Ets, Id)
  end;

get_object(Mod, Id, badarg) ->
  {badarg, Mod, Id}.

-spec cache_object(atom(), atom(), term()) -> term() | {error, term()}.
cache_object(Mod, Ets, Id) ->
  gen_server:cast(Mod, {cache, self(), Id}),
  receive
    {cached, Id} ->
      [O] = ets:lookup(Ets, Id),
      O;
    X ->
      {error, X}
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

handle_call(_Req, _From, State) ->
  {reply, ok, State}.

handle_cast({cache, ReplyTo, Id}, State) ->
  InProgress = State#state.in_progress,
  NewDict = case dict:is_key(Id, InProgress) of
              true ->
                dict:append(Id, ReplyTo);
              false ->
                Mod = State#state.mod,
                Ets = Mod,
                MState = State#state.mod_state,
                spawn(?MODULE, do_cache, [Mod, Ets, Id, MState]),
                dict:store(Id, [ReplyTo], InProgress)
            end,
  {noreply, State#state{in_progress = NewDict}};

handle_cast({cached, Id} = Msg, State) ->
  InProgress = State#state.in_progress,
  Waiters = dict:fetch(Id, InProgress),
  [W ! Msg || W <- Waiters],
  NewDict = dict:erase(Id, InProgress),
  {noreply, State#state{in_progress = NewDict}};

handle_cast({not_cached, Id, _} = Msg, State) ->
  InProgress = State#state.in_progress,
  Waiters = dict:fetch(Id, InProgress),
  [W !  Msg || W <- Waiters],
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

-spec do_cache(atom(), atom(), term(), term()) -> ok.
do_cache(Mod, Ets, Id, MState) ->
  case Mod:fetch(Id, MState) of
    {ok, Data, Expire} ->
      update_counter(Mod),
      ets:insert(Ets, new_cache_obj(Id, Data, Expire)),
      gen_server:cast(Mod, {cached, Id});
    Problem ->
      gen_server:cast(Mod, {not_cached, Id, Problem})
  end.

update_counter(Mod) ->
  Key = {Mod, date()},
  case ets:insert_new(d2api_stat, {Key, 1}) of
    true ->
      ok;
    _ ->
      ets:update_counter(d2api_stat, Key, {2, 1})
  end.
        
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
