-module (bedrock_security).
-export ([
  identify/2, 
  identify/3, 
  generate_uuid/0, 
  generate_key/1,
  invalidate_key/1,
  log/5,
  consume_test_key/1,
  must_be_at_least/2,
  must_have_access_to/3,
  must_be_able_to_read/2,
  must_be_able_to_write/2,
  must_have_service/2,
  must_be_defined/2,
  must_be_unique/3,
  must_be_associated/1,
  hash/1,
  must_be_test_key_for_service/2
]).
  
identify(admin, Key) ->
  identify(<<"administrators">>, Key);

identify(developer, Key) ->
  identify(<<"administrators">>, Key);

identify(user, Key) ->
  identify(<<"administrators">>, Key);

identify(PersonType, [{<<"email">>, Email}, {<<"password">>, Password}]) ->
  identify(PersonType, Email, Password);

identify(Table, Key) ->
  case bedrock_redis:get(Key) of
    undefined -> error;
    Id        ->
      case bedrock_pg:get(Table, Id) of
        {ok, Person} -> {ok, Person};
        {error, notfound} -> error
      end
  end.

identify(admin, Email, Password) ->
  identify(<<"administrators">>, Email, Password);

identify(developer, Email, Password) ->
  identify(<<"developers">>, Email, Password);

identify(user, Email, Password) ->
  identify(<<"users">>, Email, Password);

identify(Table, Email, Password) ->
  case bedrock_pg:find(Table, <<"email = $1">>, [Email]) of
    {ok, []}       -> error;
    {ok, [Person]} -> 
      case authenticate(Person, Password) of
        true    -> {ok, Person};
        false   -> error
      end;
    {ok, _Other}   -> error
  end.

generate_key(Person) ->
  Key = generate_uuid(),
  Id = p:id(Person),

  bedrock_redis:set(Key, Id),
  % Expire the key in one day.
  bedrock_redis:expire(Key, 3600 * 24),
  Key.

invalidate_key(Key) ->
  bedrock_redis:delete(Key).

generate_uuid() ->
  base32:encode(uuid(), [lower, nopad]).

log(admin, Who, Interface, Method, Args) ->
  % convert the args to json and then prettify it for easy reading
  Args1 = maybe_strip_password(Args),
  Json = jsx:encode(Args1),
  PrettyJson = jsx:prettify(Json),
  log(
    <<"administrator">>,
    p:id(Who),
    proplists:get_value(<<"email">>, Who),
    list_to_binary(atom_to_list(Interface)),
    list_to_binary(atom_to_list(Method)),
    PrettyJson
  );

log(developer, Who, Interface, Method, Args) ->
  Args1 = maybe_strip_password(Args),
  Json = jsx:encode(Args1),
  PrettyJson = jsx:prettify(Json),
  log(
    <<"developer">>,
    p:id(Who),
    proplists:get_value(<<"email">>, Who),
    list_to_binary(atom_to_list(Interface)),
    list_to_binary(atom_to_list(Method)),
    PrettyJson
  ).

log(ActorType, Id, Email, Interface, Method, Args) ->
  Action = [
    {<<"actor">>, ActorType},
    {<<"actor_id">>, Id},
    {<<"actor_email">>, Email},
    {<<"interface">>, Interface},
    {<<"method">>, Method},
    {<<"args">>, Args}
  ],
  bedrock_metrics:add_history_value(<<"_internal.histories.logs">>, Action).

must_be_at_least(admin, State) ->
  case proplists:get_value(role, State) of
    admin  -> ok;
    _Other -> throw(unauthorized)
  end;

must_be_at_least(developer, State) ->
  case proplists:get_value(role, State) of
    admin     -> ok;
    developer -> ok;
    _Other    -> throw(unauthorized)
  end.

must_have_access_to(admin, Target, State) ->
  Identity = p:identity(State),
  Id = p:id(Identity),
  case proplists:get_value(role, State) of
    admin ->
      case Id =:= p:id(Target) of
        true  -> ok;
        false -> throw(unauthorized)
      end;
    _Other -> throw(unauthorized)
  end;

must_have_access_to(developer, Target, State) ->
  Identity = p:identity(State),
  Id = p:id(Identity),
  case proplists:get_value(role, State) of
    admin     -> ok;
    developer ->
      case Id =:= p:id(Target) of
        true  -> ok;
        false -> throw(unauthorized)
      end;
    _Other -> throw(unauthorized)
  end;

must_have_access_to(application, Target, State) ->
  Identity = p:identity(State),
  Id = p:id(Identity),
  case proplists:get_value(role, State) of
    admin     -> ok;
    developer ->
      case Id =:= proplists:get_value(<<"developer_id">>, Target) of
        true  -> ok;
        false -> throw(unauthorized)
      end;
    application -> 
      case Id =:= p:id(Target) of
        true  -> ok;
        false -> throw(unauthorized)
      end;
    _Other -> throw(unauthorized)
  end;

must_have_access_to(user, Target, State) ->
  Identity = p:identity(State),
  Id = p:id(Identity),
  case proplists:get_value(role, State) of
    admin     -> ok;
    developer ->
      AppId = proplists:get_value(<<"application_id">>, Target),
      {ok, Application} = bedrock_pg:get(<<"applications">>, AppId),
      must_have_access_to(application, Application, State);      
    application -> 
      AppId = proplists:get_value(<<"application_id">>, Target),
      {ok, Application} = bedrock_pg:get(<<"applications">>, AppId),
      must_have_access_to(application, Application, State);
    user ->   
      case Id =:= p:id(Target) of
        true  -> ok;
        false -> throw(unauthorized)
      end;
    _Other -> throw(unauthorized)
  end;

must_have_access_to(channel, Target, State) ->
  case proplists:get_value(role, State) of
    admin       -> ok;
    developer   -> accessible(Target, developer);
    application -> accessible(Target, application);
    user        -> accessible(Target, user)
  end.

must_have_service(Service, State) ->
  Services = proplists:get_value(available_services, State),
  case Services of
    ['*'] -> ok;
    _List -> case lists:member(Service, Services) of
      true  -> ok;
      false -> throw(unavailable)
    end
  end.

hash(Pwd) ->
  {ok, Salt} = bcrypt:gen_salt(12),
  {ok, Hash} = bcrypt:hashpw(Pwd, Salt),
  Hash.

must_be_defined(Keys, Object) ->
  Undefined = [binary_to_list(Key) || Key <- Keys, 
    not proplists:is_defined(Key, Object) 
    orelse proplists:get_value(Key, Object) =:= <<"">>],

  case Undefined of
    [] -> ok;
    _  -> throw({undefined, Undefined})
  end.

must_be_unique(Table, Key, Object) ->
  case proplists:is_defined(Key, Object) of
    false -> ok;
    true  ->
      Where = io_lib:format("~s = $1", [Key]),
      Params = [proplists:get_value(Key, Object)],
      case bedrock_pg:find(Table, Where, Params) of
        {ok, []} -> ok;
        {ok, _}  -> throw({conflict, {Key, proplists:get_value(Key, Object)}})
      end
  end.

must_be_test_key_for_service(Key, ServiceId) ->
  case Key of
    undefined -> throw({requires_key, ServiceId});
    _Other    ->
      Where = <<"service_id = $1 AND key = $2">>,
      Params = [ServiceId, Key],
      case bedrock_pg:find(<<"test_access_grants">>, Where, Params) of
        {ok, []}            -> throw({requires_key, ServiceId});
        {ok, [_FoundGrant]} -> ok
      end
  end.

must_be_able_to_read(Object, State) ->
  Identity = p:identity(State),
  Id = p:id(Identity),
  case Id =:= proplists:get_value(<<"user_id">>, Object) of
    true  -> ok;
    false ->
      case proplists:get_value(<<"readers">>, Object) of
        [0]   -> ok;
        []    -> throw(unauthorized);
        Set   -> 
          case lists:member(Id, Set) of
            false -> throw(unauthorized);
            true  -> ok
          end
      end
  end.

must_be_able_to_write(Object, State) ->
  Identity = p:identity(State),
  Id = p:id(Identity),
  case Id =:= proplists:get_value(<<"user_id">>, Object) of
    true  -> ok;
    false ->
      case proplists:get_value(<<"writers">>, Object) of
        [0]   -> ok;
        []    -> throw(unauthorized);
        Set   -> 
          case lists:member(Id, Set) of
            false -> throw(unauthorized);
            true  -> ok
          end
      end
  end.

must_be_associated(State) ->
  case proplists:is_defined(application, State) of
    true  -> ok;
    false -> throw(unassociated)
  end.

consume_test_key(Key) ->
  case Key of
    undefined -> ok;
    _Other    ->
      Where = <<"key = $1">>,
      Params = [Key],
      {ok, [Grant]} = bedrock_pg:find(<<"test_access_grants">>, Where, Params),
      bedrock_pg:delete(<<"test_access_grants">>, p:id(Grant))
  end.

uuid() ->
  random:seed(now()), 
  v4(random:uniform(round(math:pow(2, 48))) - 1, 
               random:uniform(round(math:pow(2, 12))) - 1, 
               random:uniform(round(math:pow(2, 32))) - 1, 
               random:uniform(round(math:pow(2, 30))) - 1).

v4(R1, R2, R3, R4) ->
    <<R1:48, 4:4, R2:12, 2:2, R3:32, R4: 30>>.

authenticate(Person, Password) ->
  Hash = binary_to_list(proplists:get_value(<<"password">>, Person)),
  {ok, Hash} =:= bcrypt:hashpw(Password, Hash).

accessible(Channel, Role) ->
  case lists:is_member(Channel, [C || {C, _} <- protected_channels()]) of
    true  -> 
      case proplists:get_value(Channel) of
        admin        -> throw(protected);
        developer    -> case Role of
                          developer -> ok;
                          _Other    -> throw(protected)
                        end;
        application  -> case Role of
                          developer   -> ok;
                          application -> ok;
                          _Other      -> throw(protected)
                        end;
        user         -> ok
      end;
    false -> ok
  end.

protected_channels() -> [
  {<<"admin-signed-on">>, admin},
  {<<"admin-signed-off">>, admin},
  {<<"developer-created">>, admin},

  {<<"developer-signed-on">>, developer},
  {<<"developer-signed-off">>, developer}

].

maybe_strip_password([{_,_}|_]=Args) ->
  proplists:delete(<<"password">>, Args);

maybe_strip_password(Args) ->
  Args.