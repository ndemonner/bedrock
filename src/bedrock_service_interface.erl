-module (bedrock_service_interface).
-export ([
  retrieve_all/1,
  tiers/2,
  subscriber_count/2,
  tier_subscriber_count/2,
  set_testing/3,
  create/2,
  delete/2,
  add_tier/2,
  remove_tier/2
]).

retrieve_all(State) ->
  {ok, Services} = bedrock_pg:get_all(<<"services">>, <<"ORDER BY id ASC">>),
  {ok, Services, State}.

tiers(ServiceId, State) ->
  Where = <<"service_id = $1">>,
  Params = [ServiceId],
  Conditions = <<"ORDER BY tier ASC">>,
  {ok, Tiers} = bedrock_pg:find(<<"usage_constraints">>, Where, Params, Conditions),
  {ok, Tiers, State}.

subscriber_count(ServiceId, State) ->
  Where = <<"usage_constraints.service_id = $1 AND developer_usage_constraints.usage_constraint_id = usage_constraints.id">>,
  Params = [ServiceId],
  {ok, Count} = bedrock_pg:count_where(<<"developer_usage_constraints, usage_constraints">>, Where, Params),
  {ok, Count, State}.

tier_subscriber_count(TierId, State) ->
  Where = <<"developer_usage_constraints.usage_constraint_id = $1">>,
  Params = [TierId],
  {ok, Count} = bedrock_pg:count_where(<<"developer_usage_constraints">>, Where, Params),
  {ok, Count, State}.

set_testing(ServiceId, Bool, State) ->
  bedrock_security:must_be_at_least(admin, State),

  bedrock_pg:update(<<"services">>, ServiceId, [{<<"testing">>, Bool}]),

  Actor = proplists:get_value(identity, State),
  bedrock_security:log_action(admin, Actor, service, set_testing, [{service_id, ServiceId}, {testing, Bool}]),
  {ok, Service} = bedrock_pg:get(<<"services">>, ServiceId), 
  case Bool of
    true  -> bedrock_redis:publish(<<"testing-activated">>, Service);
    false -> bedrock_redis:publish(<<"testing-deactivated">>, Service)
  end,
  
  {ok, undefined, State}.

create(Service, State) ->
  bedrock_security:must_be_at_least(admin, State),

  bedrock_security:must_be_defined([<<"name">>, <<"capacity_context">>, <<"description">>], Service),
  bedrock_security:must_be_unique(<<"services">>, <<"name">>, Service),

  {ok, _Result} = bedrock_pg:insert(<<"services">>, Service),

  Actor = proplists:get_value(identity, State),
  bedrock_security:log_action(admin, Actor, service, create, Service),
  bedrock_redis:publish(<<"service-created">>, Service),

  {ok, undefined, State}.

delete(ServiceId, State) ->
  bedrock_security:must_be_at_least(admin, State),
  {ok, Service} = bedrock_pg:get(<<"services">>, ServiceId),
  bedrock_pg:delete(<<"services">>, ServiceId),

  Actor = proplists:get_value(identity, State),
  bedrock_security:log_action(admin, Actor, service, delete, Service),
  bedrock_redis:publish(<<"service-deleted">>, Service),

  {ok, undefined, State}.

add_tier(Tier, State) ->
  bedrock_security:must_be_at_least(admin, State),

  bedrock_security:must_be_defined([<<"service_id">>, <<"capacity">>], Tier),
  {ok, Tiers, _} = tiers(proplists:get_value(<<"service_id">>, Tier), State),
  Count = length(Tiers),

  Tier1 = [{<<"tier">>, Count}|Tier],
  {ok, _Result} = bedrock_pg:insert(<<"usage_constraints">>, Tier1),

  Actor = proplists:get_value(identity, State),
  bedrock_security:log_action(admin, Actor, service, add_tier, Tier1),
  bedrock_redis:publish(<<"tier-added">>, Tier1),

  {ok, undefined, State}.
  
remove_tier(TierId, State) ->
  bedrock_security:must_be_at_least(admin, State),
  {ok, Tier} = bedrock_pg:get(<<"usage_constraints">>, TierId),
  bedrock_pg:delete(<<"usage_constraints">>, TierId),

  Actor = proplists:get_value(identity, State),
  bedrock_security:log_action(admin, Actor, service, remove_tier, Tier),
  bedrock_redis:publish(<<"tier-removed">>, Tier),

  {ok, undefined, State}.

