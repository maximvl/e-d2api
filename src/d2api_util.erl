-module(d2api_util).

-export([unix_timestamp/0, is_expired/1, daystime_to_seconds/1, expire_datetime/1]).

-define(SECS_IN_DAY, 86400).
-type daystime() :: {calendar:day(), calendar:time()}.

-spec unix_timestamp() -> pos_integer().
unix_timestamp() ->
  {MS, S, _} = now(),
  (MS * 1000000) + S.

-spec is_expired(calendar:datetime()) -> boolean().
is_expired(Datetime) ->
  Datetime > calendar:universal_time().

-spec daystime_to_seconds(daystime()) -> calendar:seconds().
daystime_to_seconds({Days, Time}) ->
  calendar:time_to_seconds(Time) + (Days * ?SECS_IN_DAY).

-spec expire_datetime(daystime()) -> calendar:datetime().
expire_datetime(Expire) ->
  T1 = daystime_to_seconds(Expire),
  T2 = calendar:datetime_to_gregorian_seconds(
         calendar:universal_time()),
  calendar:gregorian_seconds_to_datetime(T1 + T2).
