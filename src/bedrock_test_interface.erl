-module (bedrock_test_interface).
-export ([
  generate_key/2
]).

generate_key(ServiceId, State) ->
  bedrock_security:must_be_at_least(admin, State),
  {ok, Service} = bedrock_pg:get(<<"services">>, ServiceId),
  Name = proplists:get_value(<<"name">>, Service),  
  Key = bedrock_security:generate_uuid(),
  FullKey = list_to_binary(io_lib:format("~s-~s", [Name, Key])),
  Grant = [
    {<<"service_id">>, ServiceId},
    {<<"key">>, FullKey}
  ],
  {ok, Result} = bedrock_pg:insert(<<"test_access_grants">>, Grant),

  Actor = proplists:get_value(identity, State),
  bedrock_security:log(admin, Actor, test, generate_key, Result),

  {ok, FullKey, State}.  