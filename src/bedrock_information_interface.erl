-module (bedrock_information_interface).
-export ([
  version/1,
  active_count/1
]).

version(State) ->
  {ok, Version} = application:get_key(bedrock, vsn),
  {ok, list_to_binary(Version), State}.

active_count(State) ->
  bedrock_security:must_be_at_least(admin, State),

  {ok, Count} = bedrock_redis:get(<<"active-persons">>),
  {ok, Count, State}.