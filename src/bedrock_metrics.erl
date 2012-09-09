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
  get_history/3
]).

reset(Name) ->
  bedrock_redis:delete(Name).

increment_counter(Name) ->
  bedrock_redis:incr(Name),
  bedrock_redis:publish(changed(Name), bedrock_redis:get(Name)).

increment_counter(Name, Amount) ->
  bedrock_redis:incrby(Name, Amount),
  bedrock_redis:publish(changed(Name), bedrock_redis:get(Name)).

decrement_counter(Name) ->
  bedrock_redis:decr(Name),
  bedrock_redis:publish(changed(Name), bedrock_redis:get(Name)).

decrement_counter(Name, Amount) ->
  bedrock_redis:decrby(Name, Amount),
  bedrock_redis:publish(changed(Name), bedrock_redis:get(Name)).

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
  add_time_series_value(Name, Value, time_series_default_trim_limit()).

add_time_series_value(Name, Value, Trim) ->
  % Milleseconds since epoch 
  Time = bedrock_metrics:unix_now() / 1000,
  Entry = [
    {<<"time">>, Time},
    {<<"value">>, Value}
  ],
  bedrock_redis:lpush(Name, term_to_binary(Entry)),
  bedrock_redis:ltrim(Name, 0, Trim),
  bedrock_redis:publish(changed(Name), Entry).

add_time_series_value_without_message(Name, Value) ->
  add_time_series_value_without_message(Name, Value, time_series_default_trim_limit()).

add_time_series_value_without_message(Name, Value, Trim) ->
  % Milleseconds since epoch 
  Time = bedrock_metrics:unix_now() / 1000,
  Entry = [
    {<<"time">>, Time},
    {<<"value">>, Value}
  ],
  bedrock_redis:lpush(Name, term_to_binary(Entry)),
  bedrock_redis:ltrim(Name, 0, Trim).

add_history_value(Name, Value) ->
  increment_counter_without_message(<<"_internal.counters.metrics">>),
  add_history_value(Name, Value, history_default_trim_limit()).

add_history_value(Name, Value, Trim) ->
  % Milleseconds since epoch 
  Time = bedrock_metrics:unix_now() / 1000,
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
  Values = bedrock_redis:lrange(Name, Start, Finish),
  [binary_to_term(Value) || Value <- Values].

unix_now() -> 
  {Mega, Sec, Micro} = erlang:now(),
  % Convert to microseconds
  Mega * 1000000 * 1000000 + Sec * 1000000 + Micro.

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

time_series_default_trim_limit() -> 256.
history_default_trim_limit()     -> 256.
