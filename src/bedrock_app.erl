-module(bedrock_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, collect_metrics/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  Ret = bedrock_sup:start_link(),
  bedrock_metrics:reset(<<"_internal.counters.connections">>),

  timer:apply_after(collection_interval() * 1000, ?MODULE, collect_metrics, []),

  Ret.

stop(_State) ->
  ok.

collect_metrics() ->
  spawn(fun() -> 
    ObjectCounter  = <<"_internal.counters.objects">>,
    ObjectTS       = <<"_internal.series.objects">>,

    MessageCounter = <<"_internal.counters.messages">>,
    MessageTS      = <<"_internal.series.messages">>,
    
    RPCCounter     = <<"_internal.counters.rpcs">>,
    RPCTS          = <<"_internal.series.rpcs">>,
    
    MetricCounter  = <<"_internal.counters.metrics">>,
    MetricTS       = <<"_internal.series.metrics">>,

    Objects = bedrock_metrics:get_then_reset_counter_value(ObjectCounter),
    bedrock_metrics:add_time_series_value(ObjectTS, Objects / collection_interval()),

    Messages = bedrock_metrics:get_then_reset_counter_value(MessageCounter),
    bedrock_metrics:add_time_series_value(MessageTS, Messages / collection_interval()),

    RPCs = bedrock_metrics:get_then_reset_counter_value(RPCCounter),
    bedrock_metrics:add_time_series_value(RPCTS, RPCs / collection_interval()),

    Metrics = bedrock_metrics:get_then_reset_counter_value(MetricCounter),
    bedrock_metrics:add_time_series_value(MetricTS, Metrics / collection_interval()),

    bedrock_redis:publish(<<"_internal.series.responses-changed">>, <<"">>),

    timer:apply_after(collection_interval() * 1000, ?MODULE, collect_metrics, [])
  end).

collection_interval() -> 10.

