-module (bedrock_information_service).
-export ([version/0, version_after_delay/1]).

%% Tomorrow: use application:vsn for the OTP version.
version() ->
  {ok, <<"0.0.1">>}.

version_after_delay(Delay) ->
  timer:sleep(Delay),
  {ok, <<"0.0.1">>}.