-module (bedrock_admin_interface).
-export ([
  sign_in/2, 
  sign_out/1,
  get_identity/1,
  establish_identity/2,
  clear_logs/1
]).

sign_in(Credentials, State) ->
  case bedrock_security:identify(admin, Credentials) of
    {ok, Person} -> 
      Key = bedrock_security:generate_key(Person),
      Reply = [{<<"identity">>, Person}, {<<"key">>, Key}],

      bedrock_security:log_action(admin, Person, admin, sign_in, []),
      
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

  bedrock_security:log_action(admin, Person, admin, sign_out, []),

  {ok, undefined, State3}.

establish_identity(Key, State) ->
  case bedrock_security:identify(admin, Key) of
    {ok, Person} ->
      Reply = [{<<"identity">>, Person}, {<<"key">>, Key}],

      IdentityT = {identity, Person},
      RoleT = {role, admin},
      KeyT = {key, Key},
      ServT = {available_services, ['*']},

      {ok, Reply, [IdentityT, RoleT, KeyT, ServT | State]};
    error -> {error, <<"You provided an invalid key.">>, State}
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