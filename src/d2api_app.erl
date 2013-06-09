-module(d2api_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  application:start(itens),
  d2api_sup:start_link().

stop(_State) ->
  ok.
