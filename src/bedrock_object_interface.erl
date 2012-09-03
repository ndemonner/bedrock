-module (bedrock_object_interface).
-export ([
  store/2,
  update/3,
  get/2,
  all_by_category/2,
  all_by_user/2,
  all_by_user_and_category/3
]).

store(Object, State) ->
  undefined.

get(ObjectId, State) ->
  case bedrock_pg:get(<<"objects">>, ObjectId) of
    {error, notfound} -> {error, <<"Could not find object with that id.">>, State};
    {ok, Object}      ->
      bedrock_security:must_be_able_to_read(Object, State),
      {ok, Object, State}
  end.

all_by_category(Category, State) ->
  {ok, Objects} = bedrock_pg:find(<<"objects">>, <<"category = $1">>, [Category]),
  lists:foreach(fun(Obj) -> 
    bedrock_security:must_be_able_to_read(Obj, State)
  end, Objects),
  {ok, Objects, State}.

all_by_user(UserId, State) ->
  {ok, Objects} = bedrock_pg:find(<<"objects">>, <<"user_id = $1">>, [UserId]),
  lists:foreach(fun(Obj) -> 
    bedrock_security:must_be_able_to_read(Obj, State)
  end, Objects),
  {ok, Objects, State}.

all_by_user_and_category(UserId, Category, State) ->
    {ok, Objects} = bedrock_pg:find(<<"objects">>, <<"user_id = $1 AND category = $2">>, [UserId, Category]),
  lists:foreach(fun(Obj) -> 
    bedrock_security:must_be_able_to_read(Obj, State)
  end, Objects),
  {ok, Objects, State}.

update(ObjectId, Changes, State) ->
  case bedrock_pg:get(<<"objects">>, ObjectId) of
    {error, notfound} -> {error, <<"Could not find object with that id.">>, State};
    {ok, Object}      ->
      bedrock_security:must_be_able_to_write(Object, State)
  end.
