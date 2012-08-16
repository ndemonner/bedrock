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
  bedrock_stats:reset_stats(),

  % Stats timers
  Interval = bedrock_stats:flush_interval() * 1000,
  timer:apply_interval(Interval, bedrock_stats, flush_counters, []),
  timer:apply_interval(Interval, bedrock_stats, flush_response_times, []),

  Ret.

stop(_State) ->
  ok.
