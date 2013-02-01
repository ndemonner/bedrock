-module (bedrock_metrics).

-export ([
  reset/1,
  unix_now/0,
  
  %% Counters
  increment_counter/1,
  increment_counter/2,  
  decrement_counter/1,
  decrement_counter/2,
  increment_counter_without_message/1,
  increment_counter_without_message/2,  
  decrement_counter_without_message/1,
  decrement_counter_without_message/2,
  get_counter_value/1,
  get_then_reset_counter_value/1,

  %% Time-series
  get_time_series_values/1,
  get_time_series_statistics/1,
  add_time_series_value/2,
  add_time_series_value/3,
  add_time_series_value_without_message/2,
  add_time_series_value_without_message/3,

  %% Histories
  add_history_value/2,
  add_history_value/3,
  get_history/1,
  get_history/2,
  get_history/3,

  %% Sorted Sets
  add_member_to_set/3,
  add_member_to_set/4,
  remove_member_from_set/2,
  get_member_rank_from_set/2,
  get_member_score_from_set/2,
  increment_member_score_in_set/3,
  get_membership_size_for_set/1,
  get_members_in_set/1,
  get_members_in_set/3 
]).

reset(Name) ->
  bedrock_redis:delete(Name).

increment_counter(Name) ->
  Val = bedrock_redis:incr(Name),
  bedrock_redis:publish(changed(Name), bedrock_redis:get(Name)),
  Val.

increment_counter(Name, Amount) ->
  Val = bedrock_redis:incrby(Name, Amount),
  bedrock_redis:publish(changed(Name), bedrock_redis:get(Name)),
  % lager:info("Publishing: ~p", [changed(Name)]),
  Val.

decrement_counter(Name) ->
  Val = bedrock_redis:decr(Name),
  bedrock_redis:publish(changed(Name), bedrock_redis:get(Name)),
  Val.

decrement_counter(Name, Amount) ->
  Val = bedrock_redis:decrby(Name, Amount),
  bedrock_redis:publish(changed(Name), bedrock_redis:get(Name)),
  Val.

increment_counter_without_message(Name) ->
  bedrock_redis:incr(Name).

increment_counter_without_message(Name, Amount) ->
  bedrock_redis:incrby(Name, Amount).

decrement_counter_without_message(Name) ->
  bedrock_redis:decr(Name).

decrement_counter_without_message(Name, Amount) ->
  bedrock_redis:decrby(Name, Amount).

get_counter_value(Name) ->
  case bedrock_redis:get(Name) of
    undefined -> 0;
    Value     -> Value
  end.

get_then_reset_counter_value(Name) ->
  case bedrock_redis:getset(Name, 0) of
    undefined -> 0;
    Value     -> Value
  end.

get_time_series_values(Name) ->
  Values = bedrock_redis:lrange(Name, 0, -1),
  [binary_to_term(Value) || Value <- Values].

get_time_series_statistics(Name) ->
  Values = get_time_series_values(Name),
  InnerValues = inner_values(Values),
  perform_statistical_analysis(InnerValues).

add_time_series_value(Name, Value) ->
  increment_counter_without_message(<<"_internal.counters.metrics">>),
  add_time_series_value(Name, Value, default_trim()).

add_time_series_value(Name, Value, Trim) ->
  % Milleseconds since epoch 
  Time = bedrock_metrics:unix_now(),
  Entry = [
    {<<"time">>, Time},
    {<<"value">>, Value}
  ],
  bedrock_redis:lpush(Name, term_to_binary(Entry)),
  bedrock_redis:ltrim(Name, 0, Trim),
  bedrock_redis:publish(changed(Name), Entry).

add_time_series_value_without_message(Name, Value) ->
  add_time_series_value_without_message(Name, Value, default_trim()).

add_time_series_value_without_message(Name, Value, Trim) ->
  % Milleseconds since epoch 
  Time = bedrock_metrics:unix_now(),
  Entry = [
    {<<"time">>, Time},
    {<<"value">>, Value}
  ],
  bedrock_redis:lpush(Name, term_to_binary(Entry)),
  bedrock_redis:ltrim(Name, 0, Trim).

add_history_value(Name, Value) ->
  increment_counter_without_message(<<"_internal.counters.metrics">>),
  add_history_value(Name, Value, default_trim()).

add_history_value(Name, Value, Trim) ->
  % Milleseconds since epoch 
  Time = bedrock_metrics:unix_now(),
  Entry = [
    {<<"time">>, Time},
    {<<"value">>, Value}
  ],
  bedrock_redis:lpush(Name, term_to_binary(Entry)),
  bedrock_redis:ltrim(Name, 0, Trim),
  bedrock_redis:publish(changed(Name), Entry).

get_history(Name) -> 
  get_history(Name, 0, -1).

get_history(Name, Limit) ->
  get_history(Name, 0, Limit).

get_history(Name, Start, Finish) ->
  % Zero-based indexes, so we subtract 1 from the finish
  Values = bedrock_redis:lrange(Name, Start, Finish - 1),
  [binary_to_term(Value) || Value <- Values].

add_member_to_set(Name, Key, Score) ->
  add_member_to_set(Name, Key, Score, default_trim()).

add_member_to_set(Name, Key, Score, Trim) ->
  increment_counter_without_message(<<"_internal.counters.metrics">>),
  bedrock_redis:zadd(Name, term_to_binary(Key), Score),
  bedrock_redis:zremrangebyrank(Name, 0, (-Trim) - 1).

remove_member_from_set(Name, Key) ->
  bedrock_redis:zrem(Name, term_to_binary(Key)).

get_member_rank_from_set(Name, Key) ->
  bedrock_redis:zrevrank(Name, term_to_binary(Key)).

get_member_score_from_set(Name, Key) ->
  bedrock_redis:zscore(Name, term_to_binary(Key)).

increment_member_score_in_set(Name, Key, Amount) ->
  increment_counter_without_message(<<"_internal.counters.metrics">>),
  bedrock_redis:zincrby(Name, Amount, term_to_binary(Key)).

get_membership_size_for_set(Name) ->
  bedrock_redis:zcard(Name).

get_members_in_set(Name) ->
  get_members_in_set(Name, 0, -1).

get_members_in_set(Name, Start, Finish) ->
  bedrock_redis:zrevrange(Name, Start, Finish).

unix_now() -> 
  {Mega, Sec, Micro} = erlang:now(),
  % Convert to milleseconds
  (Mega * 1000000 * 1000000 + Sec * 1000000 + Micro) / 1000.

%%-----------------------------------------------------------------------------
%% Private --------------------------------------------------------------------
%%-----------------------------------------------------------------------------

inner_values(Values) ->
  lists:sort([Value || [{<<"time">>, _}, {<<"value">>, Value}] <- Values]).

perform_statistical_analysis(Values) ->
  case lists:all(fun(V) -> is_number(V) end, Values) of
    true  -> continue_statistical_analysis(Values);
    false -> {error, must_be_scalar}
  end.

continue_statistical_analysis(Values) ->
  Analysis = bear:get_statistics(Values),
  [{atom_to_binary(K, utf8), V} || {K, V} <- Analysis].

changed(Name) ->
  list_to_binary(io_lib:format("~s-changed", [Name])).

default_trim() -> 256.
