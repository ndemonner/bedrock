-module (bedrock_stats).
-export ([
  store_response_time/1,
  reset_stats/0,
  message_sent/0,
  fact_stored/0,
  event_stored/0,
  rpc_made/0,
  perform_stats_aggregation/0,
  aggregation_interval/0
]).

store_response_time(State) ->
  ResStart = proplists:get_value(response_start_time, State),
  ResStop = erlang:now(),
  RoundTrip = timer:now_diff(ResStop, ResStart),

  Pid = bedrock_redis:start_transaction(),
  bedrock_redis:incrby(Pid, <<"response-time-sum">>, RoundTrip),
  bedrock_redis:incr(Pid, <<"response-total">>),
  [_Num, _Num2] = bedrock_redis:end_transaction(Pid),

  proplists:delete(response_start_time, State).

reset_stats() ->
  Pid = bedrock_redis:start_transaction(),
  bedrock_redis:set(Pid, <<"response-time-sum">>, 0),
  bedrock_redis:set(Pid, <<"response-total">>, 0),
  bedrock_redis:set(Pid, <<"message-sum">>, 0),
  bedrock_redis:set(Pid, <<"fact-sum">>, 0),
  bedrock_redis:set(Pid, <<"event-sum">>, 0),
  bedrock_redis:set(Pid, <<"rpc-sum">>, 0),
  bedrock_redis:set(Pid, <<"message-interval-total">>, 0),
  bedrock_redis:set(Pid, <<"fact-interval-total">>, 0),
  bedrock_redis:set(Pid, <<"event-interval-total">>, 0),
  bedrock_redis:set(Pid, <<"rpc-interval-total">>, 0),
  bedrock_redis:set(Pid, <<"message-interval">>, 0),
  bedrock_redis:set(Pid, <<"fact-interval">>, 0),
  bedrock_redis:set(Pid, <<"event-interval">>, 0),
  bedrock_redis:set(Pid, <<"rpc-interval">>, 0),
  bedrock_redis:end_transaction(Pid).

message_sent() ->
  bedrock_redis:incr(<<"message-interval">>).
fact_stored() ->
  bedrock_redis:incr(<<"fact-interval">>).
event_stored() ->
  bedrock_redis:incr(<<"event-interval">>).
rpc_made() ->
  bedrock_redis:incr(<<"rpc-interval">>).

perform_stats_aggregation() ->
  Pid = bedrock_redis:start_transaction(),
  bedrock_redis:getset(Pid, <<"message-interval">>, 0),
  bedrock_redis:getset(Pid, <<"fact-interval">>, 0),
  bedrock_redis:getset(Pid, <<"event-interval">>, 0),
  bedrock_redis:getset(Pid, <<"rpc-interval">>, 0),
  [MsgInt, FactInt, EvtInt, RpcInt] = bedrock_redis:end_transaction(Pid),

  Pid2 = bedrock_redis:start_transaction(),
  bedrock_redis:incrby(Pid2, <<"message-sum">>, MsgInt),
  bedrock_redis:incrby(Pid2, <<"fact-sum">>, FactInt),
  bedrock_redis:incrby(Pid2, <<"event-sum">>, EvtInt),
  bedrock_redis:incrby(Pid2, <<"rpc-sum">>, RpcInt),
  Sums = bedrock_redis:end_transaction(Pid2),

  Pid3 = bedrock_redis:start_transaction(),
  bedrock_redis:incr(Pid3, <<"message-interval-total">>),
  bedrock_redis:incr(Pid3, <<"fact-interval-total">>),
  bedrock_redis:incr(Pid3, <<"event-interval-total">>),
  bedrock_redis:incr(Pid3, <<"rpc-interval-total">>),
  Totals = bedrock_redis:end_transaction(Pid3),

  Averages = lists:zipwith(fun(Sum, Total) -> ((Sum / Total) / 10) end, Sums, Totals),
  bedrock_redis:publish(<<"general-stats">>, Averages),

  Pid4 = bedrock_redis:start_transaction(),
  bedrock_redis:get(Pid4, <<"response-time-sum">>),
  bedrock_redis:get(Pid4, <<"response-total">>),
  [RSum, RTotal] = bedrock_redis:end_transaction(Pid4),

  RTotal1 = case RTotal of 
    undefined -> 1;
    0         -> 1;
    _Other    -> RTotal
  end,

  RTime = (RSum / RTotal1) / 1000,
  bedrock_redis:publish(<<"response-average">>, RTime).

aggregation_interval() -> 10.