-module(bedrock_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  Ret = bedrock_sup:start_link(),
  bedrock_redis:set(<<"active-persons">>, 0),
  Ret.

stop(_State) ->
  ok.
