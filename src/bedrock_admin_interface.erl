-module (bedrock_admin_interface).
-export ([
  sign_in/2, 
  sign_out/1,
  get_identity/1,
  establish_identity/2,
  clear_logs/1,
  create/2
]).

create(Person, State) ->
  bedrock_security:must_be_at_least(admin, State),
  bedrock_security:must_be_defined([<<"email">>, <<"password">>], Person),
  bedrock_security:must_be_unique(<<"administrators">>, <<"email">>, Person),

  HashedPass = {<<"password">>, bedrock_security:hash(proplists:get_value(<<"password">>, Person))},
  Person1 = lists:keyreplace(<<"password">>, 1, Person, HashedPass),

  {ok, Result} = bedrock_pg:insert(<<"administrators">>, Person1),

  Actor = proplists:get_value(identity, State),
  bedrock_security:log(admin, Actor, admin, create_admin, Person1),

  bedrock_metrics:increment_counter(<<"_internal.counters.admins">>),
  bedrock_metrics:increment_counter(<<"_internal.counters.total">>),

  {ok, Result, State}.

sign_in(Credentials, State) ->
  case bedrock_security:identify(admin, Credentials) of
    {ok, Person} -> 
      Key = bedrock_security:generate_key(Person),
      Reply = [{<<"identity">>, Person}, {<<"key">>, Key}],

      bedrock_security:log(admin, Person, admin, sign_in, Person),
      bedrock_redis:publish(<<"admin-signed-in">>, Person),

      IdentityT = {identity, Person},
      RoleT = {role, admin},
      KeyT = {key, Key},
      ServT = {available_services, ['*']},

      {ok, Reply, [IdentityT, RoleT, KeyT, ServT | State]};
    error -> {error, <<"You must enter a valid set of credentials.">>, State}
  end.

sign_out(State) ->
  Person = proplists:get_value(identity, State),
  State1 = proplists:delete(identity, State),
  State2 = proplists:delete(role, State1),

  bedrock_security:invalidate_key(proplists:get_value(key, State2)),
  State3 = proplists:delete(key, State2),

  bedrock_security:log(admin, Person, admin, sign_out, Person),
  bedrock_redis:publish(<<"admin-signed-out">>, Person),

  {ok, undefined, State3}.

establish_identity(Key, State) ->
  case proplists:get_value(identity, State) of
    undefined -> 
      case bedrock_security:identify(admin, Key) of
        {ok, Person} ->
          Reply = [{<<"identity">>, Person}, {<<"key">>, Key}],

          IdentityT = {identity, Person},
          RoleT = {role, admin},
          KeyT = {key, Key},
          ServT = {available_services, ['*']},

          {ok, Reply, [IdentityT, RoleT, KeyT, ServT | State]};
        error -> {error, <<"You provided an invalid key.">>, State}
      end;
    Identity -> {ok, [{<<"identity">>, Identity}], State}
  end.
  
get_identity(State) ->
  case proplists:get_value(identity, State) of
    undefined -> {error, <<"You have not been identified by Bedrock.">>, State};
    Person    -> {ok, Person, State}
  end.

clear_logs(State) ->
  bedrock_security:must_be_at_least(admin, State),
  bedrock_security:clear_logs(),
  {ok, undefined, State}.