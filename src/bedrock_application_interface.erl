-module (bedrock_application_interface).
-export ([associate/2, create/2, get_all/1]).

associate(AppKey, OldState) ->
  OldState1 = proplists:delete(application, OldState),
  State = proplists:delete(available_services, OldState1),

  {ok, [Application]} = bedrock_pg:find(<<"applications">>, <<"appkey = $1">>, [AppKey]),
  {ok, Developer} = bedrock_pg:get(<<"developers">>, proplists:get_value(<<"developer_id">>, Application)),

  IdentityTup = {identity, Developer},

  {ok, Subs, _} = bedrock_developer_interface:subscriptions([IdentityTup]),

  Services = lists:map(fun(Sub) -> 
    {ok, Service} = bedrock_pg:get(<<"services">>, proplists:get_value(<<"service_id">>, Sub)),
    proplists:get_value(<<"name">>, Service)
  end, Subs),

  AppId = p:id(Application),
  DevId = p:id(Developer),
  Counter = p:format("developer.~w.application.~w.connections", [DevId, AppId]),
  bedrock_metrics:increment_counter(Counter),

  {ok, undefined, [{application, p:id(Application)}, {developer, p:id(Developer)}, {available_services, Services}|State]}.

create(Application, State) ->
  bedrock_security:must_be_at_least(developer, State),
  bedrock_security:must_be_unique(<<"applications">>, <<"name">>, Application),
  Dev = {<<"developer_id">>, p:id(p:identity(State))},
  Key = {<<"appkey">>, bedrock_security:generate_uuid()},
  App1 = [Dev, Key|Application],
  {ok, Saved} = bedrock_pg:insert(<<"applications">>, App1),
  {ok, Saved, State}.

get_all(State) ->
  bedrock_security:must_be_at_least(developer, State),
  {ok, Apps} = bedrock_pg:find(<<"applications">>, <<"developer_id = $1">>, [p:id(p:identity(State))]),
  {ok, Apps, State}.