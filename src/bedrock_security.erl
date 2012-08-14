-module (bedrock_security).
-export ([
  identify/2, 
  identify/3, 
  generate_uuid/0, 
  generate_key/1,
  invalidate_key/1,
  log_action/5,
  must_be_at_least/2,
  must_have_access_to/3,
  clear_logs/0
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
  Id = proplists:get_value(<<"id">>, Person),

  bedrock_redis:set(Key, Id),
  % Expire the key in one day.
  bedrock_redis:expire(Key, 3600 * 24),
  Key.

invalidate_key(Key) ->
  bedrock_redis:delete(Key).

generate_uuid() ->
  base32:encode(uuid(), [lower, nopad]).

log_action(admin, Who, Interface, Method, Args) ->
  log_action(
    <<"administrator">>,
    proplists:get_value(<<"id">>, Who),
    list_to_binary(atom_to_list(Interface)),
    list_to_binary(atom_to_list(Method)),
    list_to_binary(string:join(Args, ", "))
  );

log_action(developer, Who, Interface, Method, Args) ->
  log_action(
    <<"developer">>,
    proplists:get_value(<<"id">>, Who),
    list_to_binary(atom_to_list(Interface)),
    list_to_binary(atom_to_list(Method)),
    list_to_binary(string:join(Args, ", "))
  );

log_action(ActorType, Id, Interface, Method, Args) ->
  Action = [
    {<<"actor">>, ActorType},
    {<<"actor_id">>, Id},
    {<<"interface">>, Interface},
    {<<"method">>, Method},
    {<<"args">>, Args}
  ],
  bedrock_pg:insert('logged_actions', Action).

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
  [{<<"id">>, Id}|_] = proplists:get_value(identity, State),
  case proplists:get_value(role, State) of
    admin ->
      case Id =:= proplists:get_value(<<"id">>, Target) of
        true  -> ok;
        false -> throw(unauthorized)
      end;
    _Other -> throw(unauthorized)
  end;

must_have_access_to(developer, Target, State) ->
  [{<<"id">>, Id}|_] = proplists:get_value(identity, State),
  case proplists:get_value(role, State) of
    admin     -> ok;
    developer ->
      case Id =:= proplists:get_value(<<"id">>, Target) of
        true  -> ok;
        false -> throw(unauthorized)
      end;
    _Other -> throw(unauthorized)
  end;

must_have_access_to(application, Target, State) ->
  [{<<"id">>, Id}|_] = proplists:get_value(identity, State),
  case proplists:get_value(role, State) of
    admin     -> ok;
    developer ->
      case Id =:= proplists:get_value(<<"developer_id">>, Target) of
        true  -> ok;
        false -> throw(unauthorized)
      end;
    application -> 
      case Id =:= proplists:get_value(<<"id">>, Target) of
        true  -> ok;
        false -> throw(unauthorized)
      end;
    _Other -> throw(unauthorized)
  end;

must_have_access_to(user, Target, State) ->
  [{<<"id">>, Id}|_] = proplists:get_value(identity, State),
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
      case Id =:= proplists:get_value(<<"id">>, Target) of
        true  -> ok;
        false -> throw(unauthorized)
      end;
    _Other -> throw(unauthorized)
  end.

clear_logs() ->
  bedrock_pg:delete_all(<<"logged_actions">>).

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