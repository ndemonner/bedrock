-module (bedrock_metrics_interface).
-export ([
  reset/2,

  %% Counters
  increment_counter/2,
  increment_counter/3,  
  decrement_counter/2,
  decrement_counter/3,
  get_counter_value/2,
  get_then_reset_counter_value/2,

  %% Time-series
  get_time_series_values/2,
  get_time_series_statistics/2,
  add_time_series_value/3,
  add_time_series_value/4,

  %% Histories
  add_history_value/3,
  add_history_value/4,
  get_history/2,
  get_history/3,
  get_history/4
]).

reset(Name, State) ->
  {ok, bedrock_metrics:reset(Name), State}.

increment_counter(Name, State) ->
  {ok, bedrock_metrics:increment_counter(Name), State}.

increment_counter(Name, Amount, State) ->
  {ok, bedrock_metrics:increment_counter(Name, Amount), State}.

decrement_counter(Name, State) ->
  {ok, bedrock_metrics:decrement_counter(Name), State}.

decrement_counter(Name, Amount, State) ->
  {ok, bedrock_metrics:decrement_counter(Name, Amount), State}.

get_counter_value(Name, State) ->
  {ok, bedrock_metrics:get_counter_value(Name), State}.

get_then_reset_counter_value(Name, State) ->
  {ok, bedrock_metrics:get_then_reset_counter_value(Name), State}.

add_time_series_value(Name, Value, State) ->
  {ok, bedrock_metrics:add_time_series_value(Name, Value), State}.

add_time_series_value(Name, Value, Trim, State) ->
  {ok, bedrock_metrics:add_time_series_value(Name, Value, Trim), State}.

get_time_series_values(Name, State) ->
  Values = bedrock_metrics:get_time_series_values(Name),
  {ok, Values, State}.

get_time_series_statistics(Name, State) ->
  {ok, bedrock_metrics:get_time_series_statistics(Name), State}.

add_history_value(Name, Value, State) ->
  {ok, bedrock_metrics:add_history_value(Name, Value), State}.

add_history_value(Name, Value, Trim, State) ->
  {ok, bedrock_metrics:add_history_value(Name, Value, Trim), State}.

get_history(Name, State) -> 
  {ok, bedrock_metrics:get_history(Name), State}.

get_history(Name, Limit, State) ->
  {ok, bedrock_metrics:get_history(Name, Limit), State}.

get_history(Name, Start, Finish, State) ->
  {ok, bedrock_metrics:get_history(Name, Start, Finish), State}.