-module (bedrock_application_interface).
-export ([associate/2]).

associate(AppKey, State) ->
  {ok, [Application]} = bedrock_pg:find(<<"applications">>, <<"appkey = $1">>, [AppKey]),
  {ok, Developer} = bedrock_pg:get(<<"developers">>, proplists:get_value(<<"developer_id">>, Application)),

  IdentityTup = {identity, Developer},

  {ok, Subs, _} = bedrock_developer_interface:subscriptions([IdentityTup]),

  Services = lists:map(fun(Sub) -> 
    {ok, Service} = bedrock_pg:get(<<"services">>, proplists:get_value(<<"service_id">>, Sub)),
    proplists:get_value(<<"name">>, Service)
  end, Subs),

  {ok, undefined, [{application, p:id(Application)}, {available_services, Services}|State]}.