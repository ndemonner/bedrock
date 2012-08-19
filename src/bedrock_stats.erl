-module (bedrock_stats).
-export ([
  store_response_time/1,
  reset_stats/0,

  message_sent/0,
  fact_stored/0,
  event_stored/0,
  rpc_made/0,

  flush_counters/0,
  flush_interval/0,
  flush_response_times/0
]).

store_response_time(State) ->
  ResStart = proplists:get_value(response_start_time, State),
  ResStop = erlang:now(),
  RoundTrip = timer:now_diff(ResStop, ResStart),
  bedrock_redis:incrby(<<"response-time-sum">>, RoundTrip),
  bedrock_redis:incr(<<"response-total">>),
  proplists:delete(response_start_time, State).

reset_stats() ->
  bedrock_redis:set(<<"response-time-sum">>, 0),
  bedrock_redis:set(<<"response-total">>, 0),
  bedrock_redis:set(<<"message-sum">>, 0),
  bedrock_redis:set(<<"fact-sum">>, 0),
  bedrock_redis:set(<<"event-sum">>, 0),
  bedrock_redis:set(<<"rpc-sum">>, 0),
  bedrock_redis:set(<<"message-interval-total">>, 0),
  bedrock_redis:set(<<"fact-interval-total">>, 0),
  bedrock_redis:set(<<"event-interval-total">>, 0),
  bedrock_redis:set(<<"rpc-interval-total">>, 0),
  bedrock_redis:set(<<"message-interval">>, 0),
  bedrock_redis:set(<<"fact-interval">>, 0),
  bedrock_redis:set(<<"event-interval">>, 0),
  bedrock_redis:set(<<"rpc-interval">>, 0).

message_sent() ->
  bedrock_redis:incr(<<"message-interval">>).
fact_stored() ->
  bedrock_redis:incr(<<"fact-interval">>).
event_stored() ->
  bedrock_redis:incr(<<"event-interval">>).
rpc_made() ->
  bedrock_redis:incr(<<"rpc-interval">>).

flush_counters() ->
  MsgInt  = bedrock_redis:getset(<<"message-interval">>, 0),
  FactInt = bedrock_redis:getset(<<"fact-interval">>, 0),
  EvtInt  = bedrock_redis:getset(<<"event-interval">>, 0),
  RpcInt  = bedrock_redis:getset(<<"rpc-interval">>, 0),

  Sums = [
    bedrock_redis:incrby(<<"message-sum">>, MsgInt),
    bedrock_redis:incrby(<<"fact-sum">>, FactInt),
    bedrock_redis:incrby(<<"event-sum">>, EvtInt),
    bedrock_redis:incrby(<<"rpc-sum">>, RpcInt)
  ],
  Totals = [
    bedrock_redis:incr(<<"message-interval-total">>),
    bedrock_redis:incr(<<"fact-interval-total">>),
    bedrock_redis:incr(<<"event-interval-total">>),
    bedrock_redis:incr(<<"rpc-interval-total">>)
  ],

  Averages = lists:zipwith(fun(Sum, Total) -> ((Sum / Total) / 10) end, Sums, Totals),

  bedrock_redis:publish(<<"general-stats">>, Averages).

flush_interval() -> 10.

flush_response_times() ->
  Sum = bedrock_redis:get(<<"response-time-sum">>),
  Total = bedrock_redis:get(<<"response-total">>),

  Total1 = case Total of 
    undefined -> 1;
    0         -> 1;
    _Other    -> Total
  end,

  Time = (Sum / Total1) / 1000,

  bedrock_redis:publish(<<"response-average">>, Time).

