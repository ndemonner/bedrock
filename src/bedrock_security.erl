-module (bedrock_security).
-export ([identify/2, identify/3]).

identify(PersonType, Credentials) ->
  case Credentials of
    [{<<"email">>, Email}, {<<"password">>, Password}] -> 
      identify(PersonType, Email, Password);
    _InvalidCredentialsFormat -> error
  end.

identify(admin, Email, Password) ->
  identify(<<"admins">>, Email, Password);

identify(developer, Email, Password) ->
  identify(<<"developers">>, Email, Password);

identify(user, Email, Password) ->
  identify(<<"users">>, Email, Password);

identify(Table, Email, Password) ->
  case bedrock_pg:find(Table, <<"email = $1">>, [Email]) of
    {ok, []}       -> error;
    {ok, [Person]} -> 
      case authenticate(Person, Password) of
        ok    -> {ok, Person};
        error -> error
      end;
    {ok, _Other}   -> error
  end.

authenticate(Person, Password) ->
  Hash = proplists:get_value(<<"password">>, Person),
  {ok, Hash} =:= bcrypt:hashpw(Password, Hash).