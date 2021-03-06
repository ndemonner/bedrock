-module (bedrock_information_interface).
-export ([
  version/1
]).

version(State) ->
  {ok, Version} = application:get_key(bedrock, vsn),
  {ok, list_to_binary(Version), State}.