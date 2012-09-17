-module (bedrock_admin_interface).
-export ([
  sign_in/2, 
  sign_out/1,
  establish_identity/2,
  create/2
]).

create(Person, State) ->
  bedrock_security:must_be_at_least(admin, State),
  bedrock_security:must_be_defined([<<"email">>, <<"password">>], Person),
  bedrock_security:must_be_unique(<<"administrators">>, <<"email">>, Person),

  % Use bcrypt with 12 rounds to hash the password, and then replace the incoming plaintext with
  % the corresponding hashtext.
  HashedPass = {<<"password">>, bedrock_security:hash(proplists:get_value(<<"password">>, Person))},
  Person1 = lists:keyreplace(<<"password">>, 1, Person, HashedPass),

  % Insert a new admin row. We don't check for errors here because we've already validated
  % the admin object above.
  {ok, Result} = bedrock_pg:insert(<<"administrators">>, Person1),

  % Make sure we log this action--we don't want anyone creating admins without full
  % accountability.
  Actor = p:identity(State),
  bedrock_security:log(admin, Actor, admin, create_admin, Person1),

  bedrock_metrics:increment_counter(<<"_internal.counters.admins">>),
  bedrock_metrics:increment_counter(<<"_internal.counters.total">>),

  {ok, Result, State}.

sign_in(Credentials, State) ->
  % If bedrock security successfully authenticates based on the provided credentials,
  % we create a session key, and attach the identity of the now logged-in user to
  % the connection state.
  case bedrock_security:identify(admin, Credentials) of
    {ok, Person} -> 
      Key = bedrock_security:generate_key(Person),
      Reply = [{<<"identity">>, Person}, {<<"key">>, Key}],

      bedrock_security:log(admin, Person, admin, sign_in, Person),
      bedrock_redis:publish(<<"admin-signed-in">>, Person),

      IdentityTup = {identity, Person},
      RoleTup     = {role, admin},
      KeyTup      = {key, Key},

      % Administrators can use any service.
      ServTup     = {available_services, ['*']},

      {ok, Reply, [IdentityTup, RoleTup, KeyTup, ServTup | State]};
    error -> {error, <<"You must enter a valid set of credentials.">>, State}
  end.

sign_out(State) ->
  Person = p:identity(State),
  State1 = proplists:delete(identity, State),
  State2 = proplists:delete(role, State1),

  bedrock_security:invalidate_key(proplists:get_value(key, State2)),
  State3 = proplists:delete(key, State2),

  bedrock_security:log(admin, Person, admin, sign_out, Person),
  bedrock_redis:publish(<<"admin-signed-out">>, Person),

  {ok, undefined, State3}.

%% This method allows an already authenticated user (possessing a valid session key)
%% to associate a new connection with their identity. This way they don't have to re-sign-in
%% if they drop the connection to Bedrock momentarily.
establish_identity(Key, State) ->
  % In order to avoid needless db hits, we check to make sure that this connection
  % doesn't already have an identity associated with it.
  case p:identity(State) of
    undefined -> 
      % The rest of this mirrors admin.sign-in above.
      case bedrock_security:identify(admin, Key) of
        {ok, Person} ->
          Reply = [{<<"identity">>, Person}, {<<"key">>, Key}],

          IdentityTup = {identity, Person},
          RoleTup     = {role, admin},
          KeyTup      = {key, Key},
          ServTup     = {available_services, ['*']},

          {ok, Reply, [IdentityTup, RoleTup, KeyTup, ServTup | State]};
        error -> {error, <<"You provided an invalid key.">>, State}
      end;
    Identity -> {ok, [{<<"identity">>, Identity}], State}
  end.