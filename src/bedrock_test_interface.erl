-module (bedrock_test_interface).
-export ([
  generate_key/2
]).

generate_key(ServiceId, State) ->
  bedrock_security:must_be_at_least(admin, State),
  Key = bedrock_security:generate_uuid(),
  Grant = [
    {<<"service_id">>, ServiceId},
    {<<"key">>, Key}
  ],
  {ok, Result} = bedrock_pg:insert(<<"test_access_grants">>, Grant),

  Actor = proplists:get_value(identity, State),
  bedrock_security:log_action(admin, Actor, test, generate_key, Result),

  {ok, Key, State}.