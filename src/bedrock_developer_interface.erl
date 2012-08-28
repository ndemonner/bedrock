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
  check_email/2
]).

create(Developer, State) ->
  undefined.

delete(DeveloperId, State) ->
  undefined.

sign_in(Credentials, State) ->
  undefined.

sign_out(State) ->
  undefined.

establish_identity(Key, State) ->
  case bedrock_security:identify(developer, Key) of
    {ok, Person} ->
      Reply = [{<<"identity">>, Person}, {<<"key">>, Key}],

      IdentityTuple  = {identity, Person},
      RoleTuple      = {role, developer},
      KeyTuple       = {key, Key},

      Across         = <<"services, usage_constraints, developer_usage_constraints">>,
      Where          = <<"developer_usage_constraint.developer_id = $1 AND usage_constraints.id = developer_usage_constraints.usage_constraint_id AND service.id = usage_constraint.service_id">>,
      Params         = proplists:get_value(<<"id">>, Person),
      {ok, Services} = bedrock_pg:find(Across, Where, Params),
      ServiceAtoms   = [binary_to_atom(proplists:get_value(<<"name">>, Service), utf8) || Service <- Services],
      ServTuple      = {available_services, ServiceAtoms},

      {ok, Reply, [IdentityTuple, RoleTuple, KeyTuple, ServTuple | State]};
    error -> {error, <<"You provided an invalid key.">>, State}
  end.

get_identity(State) ->
  case proplists:get_value(identity, State) of
    undefined -> {error, <<"You have not been identified by Bedrock.">>, State};
    Person    -> {ok, Person, State}
  end.

update(Changes, State) ->
  undefined.

activate_service(ServiceId, State) ->
  undefined.

deactivate_service(ServiceId, State) ->
  undefined.

change_service_tier(ServiceId, NewTier, State) ->
  undefined.

check_email(Email, State) ->
  try bedrock_security:must_be_unique(<<"developers">>, <<"email">>, [{<<"email">>, Email}]) of
    ok -> {ok, true, State}
  catch
    throw:{conflict, _C}  -> {ok, false, State}
  end.

