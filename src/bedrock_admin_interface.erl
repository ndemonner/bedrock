-module (bedrock_admin_interface).
-export ([
  sign_in/2, 
  sign_out/1
]).

sign_in(Credentials, State) ->
  case bedrock_security:identify(admin, Credentials) of
    {ok, Admin} -> {ok, Admin, [{person, Admin}, {role, admin} | State]};
    error       -> {error, <<"You must enter a valid set of credentials.">>, State}
  end.

sign_out(State) ->
  State1 = proplists:delete(person, State),
  State2 = proplists:delete(role, State1),
  {ok, State2}.