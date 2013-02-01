-module (bedrock_service_interface).
-export ([
  retrieve_all/1,
  get_all/1,
  get/2,
  get_constraint/2,
  tiers/2,
  constraints/2,
  subscriber_count/2,
  tier_subscriber_count/2,
  constraint_subscriber_count/2,
  set_testing/3,
  create/2,
  update/3,
  delete/2,
  create_tier/2,
  delete_tier/2,
  update_tier/3
]).

get_all(State) ->
  {ok, Services} = bedrock_pg:get_all(<<"services">>, <<"ORDER BY id ASC">>),
  {ok, Services, State}.

retrieve_all(State) ->
  get_all(State).

get(Id, State) ->
  {ok, Service}  = bedrock_pg:get(<<"services">>, Id),
  {ok, Service, State}.

get_constraint(ConstraintId, State) ->
  {ok, Constraint}  = bedrock_pg:get(<<"constraints">>, ConstraintId),
  {ok, Constraint, State}.

constraints(ServiceId, State) ->
  Where = <<"service_id = $1">>,
  Params = [ServiceId],
  Conditions = <<"ORDER BY tier ASC">>,
  {ok, Constraints} = bedrock_pg:find(<<"constraints">>, Where, Params, Conditions),
  {ok, Constraints, State}.

tiers(ServiceId, State) ->
  constraints(ServiceId, State). 

subscriber_count(ServiceId, State) ->
  bedrock_security:must_be_at_least(admin, State),

  Where = <<"constraints.service_id = $1 AND subscriptions.constraint_id = constraints.id">>,
  Params = [ServiceId],
  {ok, Count} = bedrock_pg:count_where(<<"subscriptions, constraints">>, Where, Params),
  {ok, Count, State}.

constraint_subscriber_count(ConstraintId, State) ->
  bedrock_security:must_be_at_least(admin, State),

  Where = <<"subscriptions.constraint_id = $1">>,
  Params = [ConstraintId],
  {ok, Count} = bedrock_pg:count_where(<<"subscriptions">>, Where, Params),
  {ok, Count, State}.

tier_subscriber_count(ConstraintId, State) ->
  constraint_subscriber_count(ConstraintId, State).

set_testing(ServiceId, Bool, State) ->
  bedrock_security:must_be_at_least(admin, State),

  bedrock_pg:update(<<"services">>, ServiceId, [{<<"testing">>, Bool}]),

  Actor = p:identity(State),
  bedrock_security:log(admin, Actor, service, set_testing, [{service_id, ServiceId}, {testing, Bool}]),
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

  {ok, Result} = bedrock_pg:insert(<<"services">>, Service),

  Actor = p:identity(State),
  bedrock_security:log(admin, Actor, service, create, Result),
  bedrock_redis:publish(<<"service-created">>, Result),

  {ok, Result, State}.

update(ServiceId, Changes, State) ->
  bedrock_security:must_be_at_least(admin, State),
  bedrock_security:must_be_unique(<<"services">>, <<"name">>, Changes),

  {ok, _Result} = bedrock_pg:update(<<"services">>, ServiceId, Changes),

  Actor = p:identity(State),
  bedrock_security:log(admin, Actor, service, update, Changes),
  bedrock_redis:publish(<<"service-updated">>, [ServiceId, Changes]),

  {ok, Changes, State}.

delete(ServiceId, State) ->
  bedrock_security:must_be_at_least(admin, State),
  case subscriber_count(ServiceId, State) of
    {ok, 0, State} -> 
      {ok, Service} = bedrock_pg:get(<<"services">>, ServiceId),
      bedrock_pg:delete(<<"services">>, ServiceId),

      Actor = p:identity(State),
      bedrock_security:log(admin, Actor, service, delete, Service),
      bedrock_redis:publish(<<"service-deleted">>, Service),

      {ok, undefined, State};
    {ok, _, State} ->
      {error, <<"You cannot delete a service which has subscribers.">>, State}
  end.

create_tier(Tier, State) ->
  bedrock_security:must_be_at_least(admin, State),

  bedrock_security:must_be_defined([<<"service_id">>, <<"capacity">>, <<"cost">>], Tier),
  {ok, Tiers, _} = tiers(proplists:get_value(<<"service_id">>, Tier), State),
  Count = length(Tiers),

  Tier1 = [{<<"tier">>, Count}|Tier],
  {ok, Result} = bedrock_pg:insert(<<"constraints">>, Tier1),

  Actor = p:identity(State),
  bedrock_security:log(admin, Actor, service, add_tier, Result),
  bedrock_redis:publish(<<"tier-created">>, Result),

  {ok, undefined, State}.
  
delete_tier(TierId, State) ->
  bedrock_security:must_be_at_least(admin, State),

  case tier_subscriber_count(TierId, State) of
    {ok, 0, State} -> 
      {ok, Tier} = bedrock_pg:get(<<"constraints">>, TierId),
      bedrock_pg:delete(<<"constraints">>, TierId),
      Actor = p:identity(State),
      bedrock_security:log(admin, Actor, service, remove_tier, Tier),
      bedrock_redis:publish(<<"tier-deleted">>, Tier),
      {ok, undefined, State};
    {ok, _, State} ->
      {error, <<"You cannot delete a tier that already has subscribers.">>, State}
  end.

update_tier(TierId, Changes, State) ->
  bedrock_security:must_be_at_least(admin, State),

  {ok, Result} = bedrock_pg:update(<<"constraints">>, TierId, Changes),

  Actor = p:identity(State),
  bedrock_security:log(admin, Actor, service, update_tier, Changes),

  ServiceId = proplists:get_value(<<"service_id">>, Result),
  bedrock_redis:publish(<<"tier-updated">>, [ServiceId, TierId, Changes]),

  {ok, Changes, State}.


