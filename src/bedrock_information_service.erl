-module (bedrock_information_service).
-export ([version/0]).

%% Tomorrow: use application:vsn for the OTP version.
version() ->
  {ok, <<"0.0.1">>}.