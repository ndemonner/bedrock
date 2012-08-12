-module (bedrock_admin_interface).
-export ([
  sign_in/2, 
  sign_out/1
]).

%% Tomorrow: use application:vsn for the OTP version.
sign_in(Credentials, State) ->
  case Credentials of
    [{<<"email">>, Email}, {<<"password">>, Password}] -> 
      NewState = lists:keyreplace(authenticated, 1, State, {authenticated, true}),
      {ok, <<"sign_in">>, NewState};
    _InvalidObject -> 
      {error, <<"You must enter a valid set of credentials.">>, State}  
  end.

sign_out(State) ->
  {ok, State}.