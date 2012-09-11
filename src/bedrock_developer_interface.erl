-module (bedrock_developer_interface).
-export ([
  create/2,
  delete/2,
  sign_in/2,
  sign_out/1,
  get_identity/1,
  establish_identity/2,
  update/2,
  activate_service/2,
  change_service_tier/3,
  deactivate_service/2,
  subscriptions/1
]).

create(DeveloperRaw, State) ->
  bedrock_security:must_be_defined([<<"email">>, <<"password">>, <<"card">>, <<"activations">>], DeveloperRaw),
  bedrock_security:must_be_unique(<<"developers">>, <<"email">>, DeveloperRaw),  

  HashedPass = {<<"password">>, bedrock_security:hash(proplists:get_value(<<"password">>, DeveloperRaw))},
  Developer  = lists:keyreplace(<<"password">>, 1, DeveloperRaw, HashedPass),

  {value, {<<"card">>, Card}, Developer1} = lists:keytake(<<"card">>, 1, Developer),
  {value, {<<"activations">>, ServicePairs}, Developer2} = lists:keytake(<<"activations">>, 1, Developer1),

  % Validate the ServicePairs if necessary
  lists:foreach(fun({Pair}) ->
    Id            = proplists:get_value(<<"id">>, Pair),
    Key           = proplists:get_value(<<"key">>, Pair),
    {ok, Service} = bedrock_pg:get(<<"services">>, Id),
    case proplists:get_value(<<"testing">>, Service) of
      true  -> bedrock_security:must_be_test_key_for_service(Key, Id);
      false -> ok
    end
  end, ServicePairs),

  {ok, CustomerId} = bedrock_stripe:create_customer(Card, Developer2),
  Developer3       = [{<<"customer_id">>, CustomerId}|Developer2],

  {ok, Saved} = bedrock_pg:insert(<<"developers">>, Developer3),
 
  bedrock_metrics:increment_counter(<<"_internal.counters.developers">>),
  bedrock_metrics:increment_counter(<<"_internal.counters.total">>),

  % Activate the services and consume any test keys
  lists:foreach(fun(Pair) ->
    Id = proplists:get_value(<<"id">>, Pair), 
    Key = proplists:get_value(<<"key">>, Pair), 
    bedrock_service_interface:subscribe(Id, State)
  end, ServicePairs),

  % Sign them in
  Key = bedrock_security:generate_key(Saved),
  Reply = [{<<"identity">>, Saved}, {<<"key">>, Key}],

  bedrock_security:log(developer, Saved, developer, sign_in, Saved),
  bedrock_redis:publish(<<"developer-signed-in">>, Saved),

  IdentityTup = {identity, Saved},
  RoleTup = {role, developer},
  KeyTup = {key, Key},

  {ok, Subs, _} = subscriptions([IdentityTup]),
  Services = lists:map(fun(Sub) -> 
    {ok, Service} = bedrock_pg:get(<<"services">>, proplists:get_value(<<"service_id">>, Sub)),
    proplists:get_value(<<"name">>, Service)
  end, Subs),
  ServTup = {available_services, Services},

  {ok, Reply, [IdentityTup, RoleTup, KeyTup, ServTup | State]}.

delete(DeveloperId, State) ->
  undefined.

sign_in(Credentials, State) ->
  case bedrock_security:identify(developer, Credentials) of
    {ok, Person} -> 
      Key   = bedrock_security:generate_key(Person),
      Reply = [{<<"identity">>, Person}, {<<"key">>, Key}],

      bedrock_security:log(developer, Person, developer, sign_in, Person),
      bedrock_redis:publish(<<"developer-signed-in">>, Person),

      IdentityTup = {identity, Person},
      RoleTup     = {role, developer},
      KeyTup      = {key, Key},

      {ok, Subs, _} = subscriptions([IdentityTup]),

      Services = lists:map(fun(Sub) -> 
        {ok, Service} = bedrock_pg:get(<<"services">>, proplists:get_value(<<"service_id">>, Sub)),
        proplists:get_value(<<"name">>, Service)
      end, Subs),
      ServTup = {available_services, Services},

      {ok, Reply, [IdentityTup, RoleTup, KeyTup, ServTup | State]};
    error -> {error, <<"You must enter a valid set of credentials.">>, State}
  end.

sign_out(State) ->
  Person = proplists:get_value(identity, State),
  State1 = proplists:delete(identity, State),
  State2 = proplists:delete(role, State1),

  bedrock_security:invalidate_key(proplists:get_value(key, State2)),
  State3 = proplists:delete(key, State2),

  bedrock_security:log(developer, Person, developer, sign_out, Person),
  bedrock_redis:publish(<<"developer-signed-out">>, Person),

  {ok, undefined, State3}.

establish_identity(Key, State) ->
  case proplists:get_value(identity, State) of
    undefined -> 
      case bedrock_security:identify(developer, Key) of
        {ok, Person} ->
          Reply = [{<<"identity">>, Person}, {<<"key">>, Key}],

          IdentityTup  = {identity, Person},
          RoleTup      = {role, developer},
          KeyTup       = {key, Key},

          {ok, Subs, _} = subscriptions(State),
          Services = lists:map(fun(Sub) -> 
            {ok, Service} = bedrock_pg:get(<<"services">>, proplists:get_value(<<"service_id">>, Sub)),
            proplists:get_value(<<"name">>, Service)
          end, Subs),
          ServTup      = {available_services, Services},

          {ok, Reply, [IdentityTup, RoleTup, KeyTup, ServTup | State]};
        error -> {error, <<"You provided an invalid key.">>, State}
      end;
    Identity -> {ok, [{<<"identity">>, Identity}], State}
  end.

get_identity(State) ->
  case proplists:get_value(identity, State) of
    undefined -> {error, <<"You have not been identified by Bedrock.">>, State};
    Person    -> {ok, Person, State}
  end.

update(Changes, State) ->
  undefined.

activate_service(IdKeyPair, State) ->
  bedrock_security:must_be_at_least(developer, State),

  Id  = proplists:get_value(<<"id">>, IdKeyPair),
  Key = proplists:get_value(<<"key">>, IdKeyPair),

  {ok, Service} = bedrock_pg:get(<<"services">>, Id),
  case proplists:get_value(<<"testing">>, Service) of
    true  -> bedrock_security:must_be_test_key_for_service(Key, Id);
    false -> ok
  end,

  Identity = proplists:get_value(identity, State),

  Where = <<"service_id = $1 AND tier = 0">>,
  Params = [Id],
  {ok, Constraint} = bedrock_pg:find(<<"constraints">>, Where, Params),
  Sub = [
    {<<"constraint_id">>, proplists:get_value(<<"id">>, Constraint)},
    {<<"developer_id">>, proplists:get_value(<<"id">>, Identity)},
    {<<"service_id">>, Id}
  ],
  {ok, Result} = bedrock_pg:insert(<<"subscriptions">>, Sub),
  bedrock_stripe:modify_plan(proplists:get_value(<<"cost">>, Constraint), Identity),

  NewServices = [proplists:get_value(<<"name">>, Service)|proplists:get_value(available_services, State)],
  NewState    = lists:keyreplace(available_services, 1, State, {available_services, NewServices}),

  {ok, Result, State}.

deactivate_service(ServiceId, State) ->
  bedrock_security:must_be_at_least(developer, State),
  Identity    = proplists:get_value(identity, State),
  Params      = [proplists:get_value(<<"id">>, Identity), ServiceId],
  {ok, [Sub]} = bedrock_pg:find(<<"subscriptions">>, <<"developer_id = $1 AND service_id = $2">>, Params),
  bedrock_pg:delete(<<"subscriptions">>, proplists:get_value(<<"id">>, Sub)),
  bedrock_stripe:modify_plan(-(proplists:get_value(<<"cost">>, Sub)), Identity),
  {ok, undefined, State}.

change_service_tier(ServiceId, NewTier, State) ->
  bedrock_security:must_be_at_least(developer, State),
  Identity              = proplists:get_value(identity, State),
  {ok, [NewConstraint]} = bedrock_pg:find(<<"constraints">>, <<"service_id = $1 AND tier = $2">>, [ServiceId, NewTier]),
  NewConstraintId       = proplists:get_value(<<"id">>, NewConstraint),
  Where                 = <<"developer_id = $1 AND service_id = $2">>,
  Params                = [proplists:get_value(<<"id">>, Identity), ServiceId],
  {ok, [Sub]}           = bedrock_pg:find(<<"subscriptions">>, Where, Params),
  {ok, OldConstraint}   = bedrock_pg:get(<<"constraints">>, proplists:get_value(<<"constraint_id">>, Sub)),
  SubId                 = proplists:get_value(<<"id">>, Sub),
  {ok, UpdatedSub}      = bedrock_pg:update(<<"subscriptions">>, SubId, [{<<"constraint_id">>, NewConstraintId}]),
  Amount                = proplists:get_value(<<"cost">>, NewConstraint) - proplists:get_value(<<"cost">>, OldConstraint),
  bedrock_stripe:modify_plan(Amount, Identity),
  {ok, UpdatedSub, State}.

subscriptions(State) ->
  bedrock_security:must_be_at_least(developer, State),
  Identity = proplists:get_value(identity, State),
  Where = <<"id = $1">>,
  Params = [proplists:get_value(<<"id">>, Identity)],
  {ok, Subs} = bedrock_pg:find(<<"subscriptions">>, Where, Params),
  {ok, Subs, State}.