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

%% API
-export([start_link/2, get_object/2, maybe_do_cache/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SECS_IN_DAY, 86400).

-type daystime() :: {integer(), calendar:time()}.

-record(state, {ets :: atom(),
                link_fun :: fun((integer() | tuple()) -> {ok, string(), daystime()} | badarg),
                in_progress = dict:new() :: dict()}).

-record(cache_obj, {id :: integer(),
                    value :: term(),
                    expire :: calendar:datetime()
                   }).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Name, Fun) ->
  error_logger:info_report([{"starting cache", Name}]),
  gen_server:start_link({local, Name}, ?MODULE, [Name, Fun], []).

-spec get_object(atom(), term()) -> {ok, term()} | {error, term()}.
get_object(Mod, Id) ->
  Ets = Mod,
  case ets:lookup(Ets, Id) of
    [O] ->
      case is_expired(O) of
        false ->
          {ok, O#cache_obj.value};
        _ ->
          cache_object(Mod, Ets, Id)
      end;
    [] ->
      cache_object(Mod, Ets, Id)
  end.

-spec cache_object(atom(), atom(), term()) -> {ok, term()} | {error, term()}.
cache_object(Mod, Ets, Id) ->
  gen_server:cast(Mod, {cache, self(), Id}),
  receive
    {cached, Id} ->
      [O] = ets:lookup(Ets, Id),
      {ok, O};
    X ->
      {error, X}
  end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Name, Fun]) ->
  ets:new(Name, [set, named_table, public,
                {keypos, #cache_obj.id},
                {read_concurrency, true}]),
  {ok, #state{ets = Name,
              link_fun = Fun}}.

handle_call(_Req, _From, State) ->
  {reply, ok, State}.

handle_cast({cache, ReplyTo, Id}, State) ->
  InProgress = State#state.in_progress,
  NewDict = case dict:is_key(Id, InProgress) of
              true ->
                dict:append(Id, ReplyTo);
              false ->
                LinkFun = State#state.link_fun,
                Ets = State#state.ets,
                spawn(?MODULE, maybe_do_cache, [LinkFun, Ets, Id]),
                dict:store(Id, [ReplyTo], InProgress)
            end,
  {noreply, State#state{in_progress = NewDict}};

handle_cast({cached, Id} = Msg, State) ->
  InProgress = State#state.in_progress,
  Waiters = dict:fetch(Id, InProgress),
  [W ! Msg || W <- Waiters],
  NewDict = dict:erase(Id, InProgress),
  {noreply, State#state{in_progress = NewDict}};

handle_cast({not_cached, Id, Msg}, State) ->
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

-spec maybe_do_cache(atom(), atom(), term()) -> ok.
maybe_do_cache(LinkFun, Mod, Id) ->
  case LinkFun(Id) of
    {ok, Link, Expire} ->
      update_counter(Mod),
      Fetch = httpc:request(get, {Link, []}, [],
                            [{body_format, binary},
                             {full_result, false}]),
      case Fetch of
        {ok, {200, Data}} ->
          ets:insert(Mod, new_cache_obj(Id, Data, Expire)),
          gen_server:cast(Mod, {cached, Id});
        {ok, {Status, _}} ->
          gen_server:cast(Mod, {not_cached, Id, {http_status, Status}});
        Problem ->
          gen_server:cast(Mod, {not_cached, Id, Problem})
      end;
    badarg ->
      gen_server:cast(Mod, {not_cached, Id, {badarg, Mod, Id}})
  end.

update_counter(Mod) ->
  Key = {Mod, date()},
  case ets:insert_new(d2api_stat, {Key, 1}) of
    true ->
      ok;
    _ ->
      ets:update_counter(d2api_stat, Key, {2, 1})
  end.

-spec new_cache_obj(any(), term(), daystime()) -> #cache_obj{}.
new_cache_obj(Id, Obj, Expire) ->
  #cache_obj{id = Id,
             value = Obj,
             expire = expire_datetime(Expire)}.

-spec is_expired(calendar:datetime()) -> boolean().
is_expired(Obj) when is_record(Obj, cache_obj) ->
  Obj#cache_obj.expire < calendar:universal_time();

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
