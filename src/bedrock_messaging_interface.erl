-module (bedrock_messaging_interface).
-export ([subscribe/2, unsubscribe/2, publish/3]).

subscribe(Channels, State) ->
  bedrock_security:must_have_service(messaging, State),
  proplists:get_value(pid, State) ! {subscribe, Channels},
  {ok, undefined, State}.

unsubscribe(Channels, State) ->
  bedrock_security:must_have_service(messaging, State),
  proplists:get_value(pid, State) ! {unsubscribe, Channels},
  {ok, undefined, State}.

publish(Channel, Message, State) ->
  bedrock_security:must_have_service(messaging, State),

  Result = bedrock_redis:publish(Channel, term_to_binary(Message)),
  {ok, undefined, State}.