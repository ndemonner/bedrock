-module (bedrock_information_interface).
-export ([
  version/1,
  active_count/1,
  general_stats/1,
  people_stats/1
]).

version(State) ->
  {ok, Version} = application:get_key(bedrock, vsn),
  {ok, list_to_binary(Version), State}.

active_count(State) ->
  bedrock_security:must_be_at_least(admin, State),

  Count = bedrock_redis:get(<<"active-persons">>),
  {ok, Count, State}.

people_stats(State) ->
  bedrock_security:must_be_at_least(admin, State),

  % Admins
  {ok, AdminCount} = bedrock_pg:count(<<"administrators">>),
  % Devs
  {ok, DevCount} = bedrock_pg:count(<<"developers">>),
  % Apps
  {ok, AppCount} = bedrock_pg:count(<<"applications">>),
  % Users
  {ok, UserCount} = bedrock_pg:count(<<"users">>),
  % Total
  TotalCount = AdminCount + DevCount + UserCount,

  {ok, [
    {<<"admin_count">>, AdminCount}, 
    {<<"dev_count">>, DevCount}, 
    {<<"app_count">>, AppCount}, 
    {<<"user_count">>, UserCount}, 
    {<<"total_count">>, TotalCount}
  ], State}.

general_stats(State) ->
  bedrock_security:must_be_at_least(admin, State),

  RSum = bedrock_redis:get(<<"response-time-sum">>),
  RTotal = bedrock_redis:get(<<"response-total">>),

  RTime = (RSum / RTotal) / 1000,

  Sums = [
    bedrock_redis:get(<<"message-sum">>),
    bedrock_redis:get(<<"fact-sum">>),
    bedrock_redis:get(<<"event-sum">>),
    bedrock_redis:get(<<"rpc-sum">>)
  ],
  Totals = [
    bedrock_redis:get(<<"message-interval-total">>),
    bedrock_redis:get(<<"fact-interval-total">>),
    bedrock_redis:get(<<"event-interval-total">>),
    bedrock_redis:get(<<"rpc-interval-total">>)
  ],

  Totals1 = lists:map(fun(T) -> 
    case T of
      0 -> 1;
      _ -> T
    end
  end, Totals),

  Averages = lists:zipwith(fun(Sum, Total) -> ((Sum / Total) / 10) end, Sums, Totals1),
  {ok, [RTime|Averages], State}.