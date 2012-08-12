-module (bedrock_information_interface).
-export ([version/1, version_after_delay/2]).

%% Tomorrow: use application:vsn for the OTP version.
version(State) ->
  {ok, <<"0.0.1">>, State}.

version_after_delay(Delay, State) ->
  timer:sleep(Delay),
  {ok, <<"0.0.1">>, State}.