-module (bedrock_messaging_interface).
-export ([subscribe/2, unsubscribe/2, publish/3]).

subscribe(Channels, State) ->
  bedrock_security:must_have_service(<<"messaging">>, State),
  lists:foreach(fun(C) -> 
    bedrock_security:must_have_access_to(channel, C, State) 
  end, Channels),

  proplists:get_value(pid, State) ! {subscribe, Channels},
  {ok, undefined, State}.

unsubscribe(Channels, State) ->
  bedrock_security:must_have_service(<<"messaging">>, State),
  proplists:get_value(pid, State) ! {unsubscribe, Channels},
  {ok, undefined, State}.

publish(Channel, Message, State) ->
  bedrock_security:must_have_service(<<"messaging">>, State),

  bedrock_redis:publish(Channel, Message),
  {ok, undefined, State}.