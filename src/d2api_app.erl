-module(d2api_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
  application:start(inets),
  d2api:set_api_key("api_key"),
  application:start(d2api).

start(_StartType, _StartArgs) ->
  d2api_sup:start_link().

stop(_State) ->
  ok.
