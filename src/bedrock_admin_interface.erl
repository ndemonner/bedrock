-module (bedrock_admin_interface).
-export ([
  sign_in/2, 
  sign_out/1,
  get_identity/1,
  establish_identity/2
]).

sign_in(Credentials, State) ->
  case bedrock_security:identify(admin, Credentials) of
    {ok, Person} -> 
      Key = bedrock_security:generate_key(Person),
      Reply = [{<<"identity">>, Person}, {<<"key">>, Key}],
      {ok, Reply, [{identity, Person}, {role, admin}, {key, Key} | State]};
    error       -> {error, <<"You must enter a valid set of credentials.">>, State}
  end.

sign_out(State) ->
  State1 = proplists:delete(identity, State),
  State2 = proplists:delete(role, State1),

  bedrock_security:invalidate_key(proplists:get_value(key, State2)),
  State3 = proplists:delete(key, State2),

  {ok, State3}.

establish_identity(Key, State) ->
  case bedrock_security:identify(admin, Key) of
    {ok, Person} ->
      Reply = [{<<"identity">>, Person}, {<<"key">>, Key}],
      {ok, Reply, [{identity, Person}, {role, admin} | State]};
    error -> {error, <<"You provided an invalid key.">>, State}
  end.

get_identity(State) ->
  case proplists:get_value(identity, State) of
    undefined -> {error, <<"You have not been identified by Bedrock.">>, State};
    Person    -> {ok, Person, State}
  end.