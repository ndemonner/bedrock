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
  Developer = lists:keyreplace(<<"password">>, 1, DeveloperRaw, HashedPass),

  {value, {<<"card">>, Card}, Developer1} = lists:keytake(<<"card">>, 1, Developer),
  {value, {<<"activations">>, ServicePairs}, Developer2} = lists:keytake(<<"activations">>, 1, Developer1),

  % Validate the ServicePairs if necessary
  lists:foreach(fun({Pair}) ->
    Id = proplists:get_value(<<"id">>, Pair), 
    Key = proplists:get_value(<<"key">>, Pair), 
    {ok, Service} = bedrock_pg:get(<<"services">>, Id),
    case proplists:get_value(<<"testing">>, Service) of
      true  -> bedrock_security:must_be_test_key_for_service(Key, Id);
      false -> ok
    end
  end, ServicePairs),

  {ok, CustomerId} = bedrock_stripe:create_customer(Card, Developer2),
  Developer3 = [{<<"customer_id">>, CustomerId}|Developer2],

  {ok, Saved} = bedrock_pg:insert(<<"developers">>, Developer3),
 
  bedrock_metrics:increment_counter(<<"_internal.counters.developers">>),
  bedrock_metrics:increment_counter(<<"_internal.counters.total">>),

  % Activate the services and consume any test keys
  lists:foreach(fun(Pair) ->
    Id = proplists:get_value(<<"id">>, Pair), 
    Key = proplists:get_value(<<"key">>, Pair), 
    
    Where = <<"service_id = $1 AND tier = 0">>,
    Params = [Id],
    {ok, [UC]} = bedrock_pg:find(<<"usage_constraints">>, Where, Params),
    DUC = [
      {<<"developer_id">>, proplists:get_value(<<"id">>, Saved)},
      {<<"usage_constraint_id">>, proplists:get_value(<<"id">>, UC)},
      {<<"service_id">>, Id}
    ],
    bedrock_pg:insert(<<"developer_usage_constraints">>, DUC),
    bedrock_security:consume_test_key(Key)
  end, ServicePairs),

  % Sign them in
  Key = bedrock_security:generate_key(Saved),
  Reply = [{<<"identity">>, Saved}, {<<"key">>, Key}],

  bedrock_security:log(developer, Saved, developer, sign_in, Saved),
  bedrock_redis:publish(<<"developer-signed-in">>, Saved),

  IdentityT = {identity, Saved},
  RoleT = {role, developer},
  KeyT = {key, Key},

  {ok, DUCs, _} = subscriptions([IdentityT]),
  Services = lists:map(fun(DUC) -> 
    {ok, Service} = bedrock_pg:get(<<"services">>, proplists:get_value(<<"service_id">>, DUC)),
    proplists:get_value(<<"name">>, Service)
  end, DUCs),
  ServT = {available_services, Services},

  {ok, Reply, [IdentityT, RoleT, KeyT, ServT | State]}.

delete(DeveloperId, State) ->
  undefined.

sign_in(Credentials, State) ->
  case bedrock_security:identify(developer, Credentials) of
    {ok, Person} -> 
      Key = bedrock_security:generate_key(Person),
      Reply = [{<<"identity">>, Person}, {<<"key">>, Key}],

      bedrock_security:log(developer, Person, developer, sign_in, Person),
      bedrock_redis:publish(<<"developer-signed-in">>, Person),

      IdentityT = {identity, Person},
      RoleT = {role, developer},
      KeyT = {key, Key},

      {ok, DUCs, _} = subscriptions([IdentityT]),
      Services = lists:map(fun(DUC) -> 
        {ok, Service} = bedrock_pg:get(<<"services">>, proplists:get_value(<<"service_id">>, DUC)),
        proplists:get_value(<<"name">>, Service)
      end, DUCs),
      ServT = {available_services, Services},

      {ok, Reply, [IdentityT, RoleT, KeyT, ServT | State]};
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

          IdentityTuple  = {identity, Person},
          RoleTuple      = {role, developer},
          KeyTuple       = {key, Key},

          {ok, DUCs, _} = subscriptions(State),
          Services = lists:map(fun(DUC) -> 
            {ok, Service} = bedrock_pg:get(<<"services">>, proplists:get_value(<<"service_id">>, DUC)),
            proplists:get_value(<<"name">>, Service)
          end, DUCs),
          ServTuple      = {available_services, Services},

          {ok, Reply, [IdentityTuple, RoleTuple, KeyTuple, ServTuple | State]};
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

activate_service(ServiceId, State) ->
  bedrock_security:must_be_at_least(developer, State),
  Identity = proplists:get_value(identity, State),
  Where = <<"service_id = $1 AND tier = 0">>,
  Params = [ServiceId],
  {ok, UC} = bedrock_pg:find(<<"usage_constraints">>, Where, Params),
  DUC = [
    {<<"usage_constraint_id">>, proplists:get_value(<<"id">>, UC)},
    {<<"developer_id">>, proplists:get_value(<<"id">>, Identity)},
    {<<"service_id">>, ServiceId}
  ],
  {ok, Result} = bedrock_pg:insert(<<"developer_usage_constraints">>, DUC),
  bedrock_stripe:modify_plan(proplists:get_value(<<"cost">>, UC), Identity),
  {ok, Result, State}.

deactivate_service(ServiceId, State) ->
  bedrock_security:must_be_at_least(developer, State),
  Identity = proplists:get_value(identity, State),
  Params = [proplists:get_value(<<"id">>, Identity), ServiceId],
  {ok, [DUC]} = bedrock_pg:find(<<"developer_usage_constraints">>, <<"developer_id = $1 AND service_id = $2">>, Params),
  bedrock_pg:delete(<<"developer_usage_constraints">>, proplists:get_value(<<"id">>, DUC)),
  bedrock_stripe:modify_plan(-(proplists:get_value(<<"cost">>, DUC)), Identity),
  {ok, undefined, State}.

change_service_tier(ServiceId, NewTier, State) ->
  bedrock_security:must_be_at_least(developer, State),
  Identity         = proplists:get_value(identity, State),
  {ok, [NewUC]}    = bedrock_pg:find(<<"usage_constraints">>, <<"service_id = $1 AND tier = $2">>, [ServiceId, NewTier]),
  NewUCId          = proplists:get_value(<<"id">>, NewUC),
  Where            = <<"developer_id = $1 AND service_id = $2">>,
  Params           = [proplists:get_value(<<"id">>, Identity), ServiceId],
  {ok, [DUC]}      = bedrock_pg:find(<<"developer_usage_constraints">>, Where, Params),
  {ok, OldUC}      = bedrock_pg:get(<<"usage_constraints">>, proplists:get_value(<<"usage_constraint_id">>, DUC)),
  DUCId            = proplists:get_value(<<"id">>, DUC),
  {ok, UpdatedDUC} = bedrock_pg:update(<<"developer_usage_constraints">>, DUCId, [{<<"usage_constraint_id">>, NewUCId}]),
  Amount           = proplists:get_value(<<"cost">>, NewUC) - proplists:get_value(<<"cost">>, OldUC),
  bedrock_stripe:modify_plan(Amount, Identity),
  {ok, UpdatedDUC, State}.

subscriptions(State) ->
  bedrock_security:must_be_at_least(developer, State),
  Identity = proplists:get_value(identity, State),
  Where = <<"id = $1">>,
  Params = [proplists:get_value(<<"id">>, Identity)],
  {ok, DUCs} = bedrock_pg:find(<<"developer_usage_constraints">>, Where, Params),
  {ok, DUCs, State}.