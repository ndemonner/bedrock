-module (bedrock_service_interface).
-export ([
  retrieve_all/1,
  tiers/2,
  subscriber_count/2,
  set_testing/3
]).

retrieve_all(State) ->
  {ok, Services} = bedrock_pg:get_all(<<"services">>, <<"ORDER BY name ASC">>),
  {ok, Services, State}.

tiers(ServiceId, State) ->
  Where = <<"service_id = $1">>,
  Params = [ServiceId],
  Conditions = <<"ORDER BY level ASC">>,
  {ok, Tiers} = bedrock_pg:find(<<"usage_constraints">>, Where, Params, Conditions),
  {ok, Tiers, State}.

subscriber_count(ServiceId, State) ->
  % Where = <<"usage_constraints.service_id = $1 AND developer_usage_constraints.usage_constraint_id = usage_constraints.id">>,
  % Params = [ServiceId],
  % {ok, Count} = bedrock_pg:count_where(<<"developer_usage_constraints, usage_constraints">>, Where, Params),
  {ok, 0, State}.

set_testing(ServiceId, Bool, State) ->
  bedrock_security:must_be_at_least(admin, State),

  bedrock_pg:update(<<"services">>, ServiceId, [{<<"testing">>, Bool}]),

  %% figure out whats wrong here
  Actor = proplists:get_value(identity, State),
  bedrock_security:log_action(admin, Actor, service, set_testing, [ServiceId, Bool]),

  {ok, undefined, State}.