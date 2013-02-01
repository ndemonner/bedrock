-module (bedrock_messaging_interface).
-export ([subscribe/2, unsubscribe/2, publish/3]).

subscribe(Channels, State) ->
  bedrock_security:must_have_service(<<"messaging">>, State),
  Channels1 = [scope(C, State) || C <- Channels],
  lists:foreach(fun(C) -> 
    bedrock_security:must_have_access_to(channel, C, State) 
  end, Channels1),

  % lager:info("Subscribing to: ~p", [Channels1]),

  proplists:get_value(pid, State) ! {subscribe, Channels1},
  {ok, Channels1, State}.

unsubscribe(Channels, State) ->
  bedrock_security:must_have_service(<<"messaging">>, State),
  Channels1 = [scope(C, State) || C <- Channels],
  proplists:get_value(pid, State) ! {unsubscribe, Channels1},
  {ok, undefined, State}.

%% F&F
publish(Channel, Message, State) ->
  bedrock_security:must_have_service(<<"messaging">>, State),
  bedrock_redis:publish(scope(Channel, State), Message),
  bedrock_meter:increment(<<"messaging">>, 1),
  {ok, undefined, State}.

scope(Name, State) ->
  Role = proplists:get_value(role, State),
  Id   = proplists:get_value(<<"id">>, proplists:get_value(identity, State)),
  case Role of
    admin       -> Name;
    developer   -> 
      list_to_binary(io_lib:format("developer.~w.~s", [Id, Name]));
    application -> 
      list_to_binary(io_lib:format("application.~w.~s", [Id, Name]));    
    user        -> 
      list_to_binary(io_lib:format("application.~w.~s", [p:app(State), Name]));
    undefined   ->
      case proplists:get_value(application, State) of
        undefined   -> throw(unauthorized);
        Application -> 
          AppId = proplists:get_value(<<"id">>, Application),
          list_to_binary(io_lib:format("application.~w.~s", [AppId, Name]))
      end
  end.
