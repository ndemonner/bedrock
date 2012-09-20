-module (bedrock_developer_interface).
-export ([
  create/2,
  delete/1,
  sign_in/2,
  sign_out/1,
  get_identity/1,
  establish_identity/2,
  update/2,
  activate_service/2,
  change_subscription/3,
  deactivate_service/2,
  subscriptions/1,
  subscriptions_for_customer/2
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
    Id            = p:id(Pair),
    Key           = proplists:get_value(<<"key">>, Pair),
    {ok, Service} = bedrock_pg:get(<<"services">>, Id),
    case proplists:get_value(<<"testing">>, Service) of
      true  -> bedrock_security:must_be_test_key_for_service(Key, Id);
      false -> ok
    end
  end, ServicePairs),

  {ok, Saved} = bedrock_pg:insert(<<"developers">>, Developer2),
  bedrock_redis:publish(<<"developer-created">>, [{<<"card">>, Card}|Saved]),
 
  bedrock_metrics:increment_counter(<<"_internal.counters.developers">>),
  bedrock_metrics:increment_counter(<<"_internal.counters.total">>),

  % Activate the services and consume any test keys
  lists:foreach(fun(Pair) ->
    Id = p:id(Pair), 
    Key = proplists:get_value(<<"key">>, Pair), 
    Where = <<"service_id = $1 AND tier = 0">>,
    Params = [Id],
    {ok, Constraint} = bedrock_pg:find(<<"constraints">>, Where, Params),
    Sub = [
      {<<"constraint_id">>, p:id(Constraint)},
      {<<"developer_id">>, p:id(Saved)},
      {<<"service_id">>, Id},
      {<<"cost">>, proplists:get_value(<<"cost">>, Constraint)}
    ],
    bedrock_pg:insert(<<"subscriptions">>, Sub),
    bedrock_security:consume_test_key(Key)
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

delete(State) ->
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
  Person = p:identity(State),
  State1 = proplists:delete(identity, State),
  State2 = proplists:delete(role, State1),

  bedrock_security:invalidate_key(proplists:get_value(key, State2)),
  State3 = proplists:delete(key, State2),

  bedrock_security:log(developer, Person, developer, sign_out, Person),
  bedrock_redis:publish(<<"developer-signed-out">>, Person),

  {ok, undefined, State3}.

establish_identity(Key, State) ->
  case p:identity(State) of
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
  case p:identity(State) of
    undefined -> {error, <<"You have not been identified by Bedrock.">>, State};
    Person    -> {ok, Person, State}
  end.

update(Changes, State) ->
  undefined.

activate_service(IdKeyPair, State) ->
  bedrock_security:must_be_at_least(developer, State),

  Id  = p:id(IdKeyPair),
  Key = proplists:get_value(<<"key">>, IdKeyPair),

  {ok, Service} = bedrock_pg:get(<<"services">>, Id),
  case proplists:get_value(<<"testing">>, Service) of
    true  -> 
      bedrock_security:must_be_test_key_for_service(Key, Id),
      bedrock_security:consume_test_key(Key);
    false -> ok
  end,

  Identity = p:identity(State),

  Where = <<"service_id = $1 AND tier = 0">>,
  Params = [Id],
  {ok, Constraint} = bedrock_pg:find(<<"constraints">>, Where, Params),
  Sub = [
    {<<"constraint_id">>, p:id(Constraint)},
    {<<"developer_id">>, p:id(Identity)},
    {<<"service_id">>, Id},
    {<<"cost">>, proplists:get_value(<<"cost">>, Constraint)}
  ],
  {ok, Result} = bedrock_pg:insert(<<"subscriptions">>, Sub),

  NewServices = [proplists:get_value(<<"name">>, Service)|proplists:get_value(available_services, State)],
  NewState    = lists:keyreplace(available_services, 1, State, {available_services, NewServices}),

  {ok, Result, State}.

deactivate_service(ServiceId, State) ->
  bedrock_security:must_be_at_least(developer, State),
  Identity    = p:identity(State),
  Params      = [p:id(Identity), ServiceId],
  {ok, [Sub]} = bedrock_pg:find(<<"subscriptions">>, <<"developer_id = $1 AND service_id = $2">>, Params),
  bedrock_pg:delete(<<"subscriptions">>, p:id(Sub)),

  {ok, Service} = bedrock_pg:get(<<"service">>, ServiceId),
  NewServices = proplists:delete(proplists:get_value(<<"name">>, Service), proplists:get_value(available_services, State)),
  NewState    = lists:keyreplace(available_services, 1, State, {available_services, NewServices}),

  {ok, undefined, State}.

change_subscription(ServiceId, NewTier, State) ->
  bedrock_security:must_be_at_least(developer, State),
  Identity              = p:identity(State),
  {ok, [NewConstraint]} = bedrock_pg:find(<<"constraints">>, <<"service_id = $1 AND tier = $2">>, [ServiceId, NewTier]),
  NewCost               = proplists:get_value(<<"cost">>, NewConstraint),
  NewConstraintId       = p:id(NewConstraint),
  Where                 = <<"developer_id = $1 AND service_id = $2">>,
  Params                = [p:id(Identity), ServiceId],
  {ok, [Sub]}           = bedrock_pg:find(<<"subscriptions">>, Where, Params),
  SubId                 = p:id(Sub),
  Changes               = [{<<"constraint_id">>, NewConstraintId}, {<<"cost">>, NewCost}],
  {ok, UpdatedSub}      = bedrock_pg:update(<<"subscriptions">>, SubId, Changes),
  {ok, UpdatedSub, State}.

subscriptions(State) ->
  bedrock_security:must_be_at_least(developer, State),
  Identity = p:identity(State),
  Where = <<"developer_id = $1">>,
  Params = [p:id(Identity)],
  {ok, Subs} = bedrock_pg:find(<<"subscriptions">>, Where, Params),
  {ok, Subs, State}.

subscriptions_for_customer(Id, State) ->
  bedrock_security:must_be_at_least(developer, State),
  WhereDev = <<"customer_id = $1">>,
  ParamsDev = [Id],
  {ok, Developer} = bedrock_pg:find(<<"developers">>, WhereDev, ParamsDev),

  Where = <<"developer_id = $1">>,
  Params = [p:id(Developer)],
  {ok, Subs} = bedrock_pg:find(<<"subscriptions">>, Where, Params),
  {ok, Subs, State}.
