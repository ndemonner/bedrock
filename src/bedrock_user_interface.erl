-module (bedrock_user_interface).
-export ([
  create/2,
  delete/1,
  sign_in/2,
  sign_out/1,
  update/2,
  establish_identity/2
]).

create(Person, State) ->
  bedrock_security:must_be_associated(State),
  bedrock_security:must_be_defined([<<"email">>, <<"password">>], Person),

  % Use bcrypt with 12 rounds to hash the password, and then replace the incoming plaintext with
  % the corresponding hashtext.
  HashedPass = {<<"password">>, bedrock_security:hash(proplists:get_value(<<"password">>, Person))},
  Person1 = lists:keyreplace(<<"password">>, 1, Person, HashedPass),

  % Insert a new user row. We don't check for errors here because we've already validated
  % the user object above.
  {ok, Result} = bedrock_pg:insert(<<"users">>, Person1),

  bedrock_metrics:increment_counter(<<"_internal.counters.users">>),
  bedrock_metrics:increment_counter(<<"_internal.counters.total">>),

  {ok, Result, State}.

sign_in(Credentials, State) ->
  bedrock_security:must_be_associated(State),

  % If bedrock security successfully authenticates based on the provided credentials,
  % we create a session key, and attach the identity of the now logged-in user to
  % the connection state.
  case bedrock_security:identify(user, Credentials) of
    {ok, Person} -> 
      Key = bedrock_security:generate_key(Person),
      Reply = [{<<"identity">>, Person}, {<<"key">>, Key}],

      IdentityTup = {identity, Person},
      RoleTup     = {role, user},
      KeyTup      = {key, Key},

      {ok, Reply, [IdentityTup, RoleTup, KeyTup | State]};
    error -> {error, <<"You must enter a valid set of credentials.">>, State}
  end.

sign_out(State) ->
  bedrock_security:must_be_associated(State),

  State1 = proplists:delete(identity, State),
  State2 = proplists:delete(role, State1),

  bedrock_security:invalidate_key(proplists:get_value(key, State2)),
  State3 = proplists:delete(key, State2),

  {ok, undefined, State3}.

update(Changes, State) ->
  undefined.

delete(State) ->
  bedrock_security:must_be_associated(State),

  Identity = p:identity(State),
  Id = p:id(Identity),

  bedrock_object_interface:delete_all_by_user(Id, State),
  bedrock_pg:delete(<<"users">>, Id),

  {ok, undefined, NewState} = sign_out(State),
  {ok, undefined, NewState}.

establish_identity(Key, State) ->
  bedrock_security:must_be_associated(State),

  case p:identity(State) of
    undefined -> 
      case bedrock_security:identify(user, Key) of
        {ok, Person} ->
          Reply = [{<<"identity">>, Person}, {<<"key">>, Key}],

          IdentityTup  = {identity, Person},
          RoleTup      = {role, developer},
          KeyTup       = {key, Key},

          {ok, Reply, [IdentityTup, RoleTup, KeyTup | State]};
        error -> {error, <<"You provided an invalid key.">>, State}
      end;
    Identity -> {ok, [{<<"identity">>, Identity}], State}
  end.