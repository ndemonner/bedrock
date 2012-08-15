-module (bedrock_log_interface).
-export ([
  view_all/3, 
  view_scoped_to_interface/4, 
  view_scoped_to_admin/4,
  view_scoped_to_developer/4
]).

view_all(Limit, Page, State) ->
  bedrock_security:must_be_at_least(admin, State),

  % Subtract one from the page to get the correct offset. E.g., a Limit of 25,
  % and a Page of 4 will start the results at row 100.
  Offset = Limit * (Page - 1),

  Conditions = io_lib:format("ORDER BY created DESC LIMIT ~w OFFSET ~w", [Limit, Offset]),
  {ok, Results} = bedrock_pg:get_all(<<"logged_actions">>, Conditions),
  {ok, Results, State}.

view_scoped_to_interface(Interface, Limit, Page, State) ->
  bedrock_security:must_be_at_least(admin, State),

  % Subtract one from the page to get the correct offset. E.g., a Limit of 25,
  % and a Page of 4 will start the results at row 100.
  Offset = Limit * (Page - 1),

  Where = "interface = $1",
  Params = [Interface],
  Conditions = io_lib:format("ORDER BY created DESC LIMIT ~w OFFSET ~w", [Limit, Offset]),
  {ok, Results} = bedrock_pg:find(<<"logged_actions">>, Where, Params, Conditions),
  {ok, Results, State}.

view_scoped_to_admin(Person, Limit, Page, State) ->
  bedrock_security:must_be_at_least(admin, State),
  bedrock_security:must_have_access_to(admin, Person, State),

  % Subtract one from the page to get the correct offset. E.g., a Limit of 25,
  % and a Page of 4 will start the results at row 100.
  Offset = Limit * (Page - 1),

  Where = "actor = $1 AND actor_id = $2",
  Params = [<<"administrator">>, proplists:get_value(<<"id">>, Person)],
  Conditions = io_lib:format("ORDER BY created DESC LIMIT ~w OFFSET ~w", [Limit, Offset]),
  {ok, Results} = bedrock_pg:find(<<"logged_actions">>, Where, Params, Conditions),
  {ok, Results, State}.

view_scoped_to_developer(Person, Limit, Page, State) ->
  bedrock_security:must_be_at_least(developer, State),
  bedrock_security:must_have_access_to(developer, Person, State),

  % Subtract one from the page to get the correct offset. E.g., a Limit of 25,
  % and a Page of 4 will start the results at row 100.
  Offset = Limit * (Page - 1),

  Where = "actor = $1 AND actor_id = $2",
  Params = [<<"developer">>, proplists:get_value(<<"id">>, Person)],
  Conditions = io_lib:format("ORDER BY created DESC LIMIT ~w OFFSET ~w", [Limit, Offset]),
  {ok, Results} = bedrock_pg:find(<<"logged_actions">>, Where, Params, Conditions),
  {ok, Results, State}.
