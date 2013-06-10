-module(d2api_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
  application:start(inets),
  application:start(d2api).

start(_StartType, _StartArgs) ->
  case application:get_env(api_key_file) of
    {ok, File} ->
      {ok, BinKey} = file:read_file(filename:absname(File)),
      Key = binary_to_list(BinKey),
      d2api:set_api_key(Key),
      error_logger:info_report([{"api key", Key}]);
    _ ->
      ok
  end,
  d2api_sup:start_link().

stop(_State) ->
  ok.
