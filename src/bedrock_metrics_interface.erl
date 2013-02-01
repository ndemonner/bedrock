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
  get_history/4,

  %% Sorted sets
  add_member_to_set/4,
  add_member_to_set/5,
  remove_member_from_set/3,
  get_member_rank_from_set/3,
  get_member_score_from_set/3,
  increment_member_score_in_set/4,
  get_membership_size_for_set/2,
  get_members_in_set/2,
  get_members_in_set/4
]).

%%% All metrics calls are intended to be used as notifications (fire and forget).
reset(Name, State) ->
  {ok, bedrock_metrics:reset(scope(Name, State)), State}.

increment_counter(Name, State) ->
  {ok, bedrock_metrics:increment_counter(scope(Name, State)), State}.

increment_counter(Name, Amount, State) ->
  {ok, bedrock_metrics:increment_counter(scope(Name, State), Amount), State}.

decrement_counter(Name, State) ->
  {ok, bedrock_metrics:decrement_counter(scope(Name, State)), State}.

decrement_counter(Name, Amount, State) ->
  {ok, bedrock_metrics:decrement_counter(scope(Name, State), Amount), State}.

get_counter_value(Name, State) ->
  {ok, bedrock_metrics:get_counter_value(scope(Name, State)), State}.

get_then_reset_counter_value(Name, State) ->
  {ok, bedrock_metrics:get_then_reset_counter_value(scope(Name, State)), State}.

add_time_series_value(Name, Value, State) ->
  {ok, bedrock_metrics:add_time_series_value(scope(Name, State), Value), State}.

add_time_series_value(Name, Value, Trim, State) ->
  {ok, bedrock_metrics:add_time_series_value(scope(Name, State), Value, Trim), State}.

get_time_series_values(Name, State) ->
  Values = bedrock_metrics:get_time_series_values(scope(Name, State)),
  {ok, Values, State}.

get_time_series_statistics(Name, State) ->
  {ok, bedrock_metrics:get_time_series_statistics(scope(Name, State)), State}.

add_history_value(Name, Value, State) ->
  {ok, bedrock_metrics:add_history_value(scope(Name, State), Value), State}.

add_history_value(Name, Value, Trim, State) ->
  {ok, bedrock_metrics:add_history_value(scope(Name, State), Value, Trim), State}.

get_history(Name, State) -> 
  {ok, bedrock_metrics:get_history(scope(Name, State)), State}.

get_history(Name, Limit, State) ->
  {ok, bedrock_metrics:get_history(scope(Name, State), Limit), State}.

get_history(Name, Start, Finish, State) ->
  {ok, bedrock_metrics:get_history(scope(Name, State), Start, Finish), State}.

add_member_to_set(Name, Key, Score, State) ->
  {ok, bedrock_metrics:add_member_to_set(scope(Name, State), Key, Score), State}.

add_member_to_set(Name, Key, Score, Trim, State) ->
  {ok, bedrock_metrics:add_member_to_set(scope(Name, State), Key, Score, Trim), State}.

remove_member_from_set(Name, Key, State) ->
  {ok, bedrock_metrics:remove_member_from_set(scope(Name, State), Key), State}.

get_member_rank_from_set(Name, Key, State) ->
  {ok, bedrock_metrics:get_member_rank_from_set(scope(Name, State), Key), State}.

get_member_score_from_set(Name, Key, State) ->
  {ok, bedrock_metrics:get_member_score_from_set(scope(Name, State), Key), State}.

increment_member_score_in_set(Name, Key, Amount, State) ->
  {ok, bedrock_metrics:increment_member_score_in_set(scope(Name, State), Key, Amount), State}.

get_membership_size_for_set(Name, State) ->
  {ok, bedrock_metrics:get_membership_size_for_set(scope(Name, State)), State}.

get_members_in_set(Name, State) ->
  {ok, bedrock_metrics:get_members_in_set(scope(Name, State)), State}.

get_members_in_set(Name, Start, Finish, State) ->
  {ok, bedrock_metrics:get_members_in_set(scope(Name, State), Start, Finish), State}.

scope(Name, State) ->
  Role = proplists:get_value(role, State),
  Id   = proplists:get_value(<<"id">>, proplists:get_value(identity, State)),
  case Role of
    admin       -> Name;
    developer   -> 
      list_to_binary(io_lib:format("developer.~w.~s", [Id, Name]));
    application -> 
      list_to_binary(io_lib:format("application.~w.~s", [Id, Name]));    
    user        -> 
      list_to_binary(io_lib:format("user.~w.~s", [Id, Name]));
    undefined   ->
      case proplists:get_value(application, State) of
        undefined   -> throw(unauthorized);
        Application -> 
          AppId = proplists:get_value(<<"id">>, Application),
          list_to_binary(io_lib:format("application.~w.~s", [AppId, Name]))
      end
  end.


