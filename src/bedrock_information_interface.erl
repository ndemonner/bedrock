-module (bedrock_information_interface).
-export ([
  version/1,
  persons_connected_count/1,
  admin_count/1,
  developer_count/1,
  user_count/1
]).

version(State) ->
  {ok, Version} = application:get_key(bedrock, vsn),
  {ok, list_to_binary(Version), State}.

persons_connected_count(State) ->
  bedrock_security:must_be_at_least(admin, State),

  Count = bedrock_redis:get(<<"persons-connected">>),
  {ok, Count, State}.

admin_count(State) ->
  bedrock_security:must_be_at_least(admin, State),

  {ok, Count} = bedrock_pg:count(<<"administrators">>),
  {ok, Count, State}.

developer_count(State) ->
  bedrock_security:must_be_at_least(admin, State),

  {ok, Count} = bedrock_pg:count(<<"developers">>),
  {ok, Count, State}.

user_count(State) ->
  bedrock_security:must_be_at_least(admin, State),

  {ok, Count} = bedrock_pg:count(<<"users">>),
  {ok, Count, State}.