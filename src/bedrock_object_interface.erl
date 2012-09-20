-module (bedrock_object_interface).
-export ([
  create/3,
  update_attributes/3,
  delete/2,
  remove_reader/3,
  remove_writer/3,
  add_writer/3,
  add_reader/3,
  get/2,
  all/2,
  one/2,
  all_by_user/2,
  all_by_user_with_category/3
]).

create(Category, Attributes, State) ->
  bedrock_security:must_be_associated(State),

  AppId = p:app(State),
  ObjectId = bedrock_security:generate_uuid(),

  Time = unix_now(),

  Object = [
    {<<"id">>, ObjectId},
    {<<"category">>, Category},
    {<<"application_id">>, AppId},
    {<<"attributes">>, Attributes},
    {<<"created">>, Time},
    {<<"updated">>, Time},
    {<<"readers">>, []},
    {<<"writers">>, []}
  ],

  CategoryKey = key(Category, AppId),
  CoreKey = key(<<"core">>, ObjectId),

  % now we break it back up for storage
  Worker = bedrock_redis:start_transaction(),
  bedrock_redis:sadd(Worker, CategoryKey, ObjectId),
  bedrock_redis:hmset(Worker, CoreKey, [
    <<"category">>, Category,
    <<"application_id">>, AppId,
    <<"created">>, Time,
    <<"updated">>, Time,
  ]),
  AttrList = lists:flatten([[K,V]||{K,V} <- Attributes]),
  bedrock_redis:hmset(Worker, key(<<"attributes">>, ObjectId), AttrList),
  bedrock_redis:end_transaction(Worker),

  Object1 = case p:id(p:identity(State)) of
    undefined -> Object;
    UserId    -> 
      bedrock_redis:hset(CoreKey, <<"user_id">>, UserId),
      UserKey = key(<<"objects">>, UserId),
      UserAndCatKey = key(CategoryKey, UserId),
      bedrock_redis:sadd(UserKey, ObjectId),
      bedrock_redis:sadd(UserAndCatKey, ObjectId),
      [{<<"user_id">>, UserId}|Object]
  end,

  % Increment the applicaiton-keyd object count
  Counter = list_to_binary(io_lib:format("_internal.counters.objects.~w", [AppId])),
  bedrock_metrics:increment_counter_without_message(Counter).

  % Increment the time-series dependent counter
  bedrock_metrics:increment_counter_without_message(<<"_internal.counters.objects">>),
  % and the overall total object count in the system
  bedrock_metrics:increment_counter_without_message(<<"_internal.counters.objects.total">>),

  bedrock_meter:adjust_usage(base, erlang:byte_size(term_to_binary(Object1))),

  {ok, Object1, State}.

update_attributes(Id, Changes, State) ->
  bedrock_security:must_be_associated(State),

  Object = construct_object(Id),
  bedrock_security:must_be_able_to_write(Object, State),

  OldSize = erlang:byte_size(term_to_binary(Object)),

  ChangeList = lists:flatten([[K,V]||{K,V} <- Changes]),
  bedrock_redis:hmset(Worker, key(<<"attributes">>, Id), ChangeList),

  % Instead of reconstructing the object from Redis again (which could be costly)
  % just merge the changes into the existing object
  OldAttrs = proplists:get_value(<<"attributes">>, Object),
  SChanges = lists:keysort(1, Changes),
  SOldAttrs = lists:keysort(1, OldAttrs),
  NewAttrs = lists:keymerge(1, SChanges, SOldAttrs),
  UpdatedObject = lists:keyreplace(<<"attributes">>, 1, Object, {<<"attributes">>, NewAttrs}),

  NewSize = erlang:byte_size(term_to_binary(UpdatedObject)),

  SizeChange = NewSize - OldSize,
  bedrock_meter:adjust_usage(base, SizeChange),

  {ok, UpdatedObject, State}.

delete(Id, State) ->
  bedrock_security:must_be_associated(State),

  Object = construct_object(Id),
  bedrock_security:must_be_able_to_write(Object, State),

  destroy_object(Object),

  Counter = list_to_binary(io_lib:format("_internal.counters.objects.~w", [p:app(State)])),
  bedrock_metrics:decrement_counter_without_message(Counter).

  bedrock_metrics:decrement_counter_without_message(<<"_internal.counters.objects.total">>),

  bedrock_meter:adjust_usage(base, -(erlang:byte_size(term_to_binary(Object)))),

  {ok, undefined, State}.

remove_reader(UserId, ObjectId, State) ->
  bedrock_security:must_be_associated(State),

  Object = construct_object(ObjectId),
  bedrock_security:must_be_able_to_write(Object, State),

  bedrock_redis:srem(key(<<"readers">>, ObjectId), UserId),

  bedrock_meter:adjust_usage(base, -(erlang:byte_size(term_to_binary(UserId)))),

  {ok, undefined, State}.

remove_writer(UserId, ObjectId, State) ->
  bedrock_security:must_be_associated(State),

  Object = construct_object(ObjectId),
  bedrock_security:must_be_able_to_write(Object, State),

  bedrock_redis:srem(key(<<"writers">>, ObjectId), UserId),

  bedrock_meter:adjust_usage(base, -(erlang:byte_size(term_to_binary(UserId)))),

  {ok, undefined, State}.

add_reader(UserId, ObjectId, State) ->
  bedrock_security:must_be_associated(State),

  Object = construct_object(ObjectId),
  bedrock_security:must_be_able_to_write(Object, State),

  bedrock_redis:sadd(key(<<"readers">>, ObjectId), UserId),

  bedrock_meter:adjust_usage(base, erlang:byte_size(term_to_binary(UserId))),

  {ok, undefined, State}.

add_writer(UserId, ObjectId, State) ->
  bedrock_security:must_be_associated(State),

  Object = construct_object(ObjectId),
  bedrock_security:must_be_able_to_write(Object, State),

  bedrock_redis:sadd(key(<<"writers">>, ObjectId), UserId),

  bedrock_meter:adjust_usage(base, erlang:byte_size(term_to_binary(UserId))),

  {ok, undefined, State}.

get(Id, State) ->
  bedrock_security:must_be_associated(State),

  Object = construct_object(ObjectId),
  bedrock_security:must_be_able_to_read(Object, State),

  {ok, Object, State}.

all(Category, State) ->
  bedrock_security:must_be_associated(State),

  Ids = bedrock_redis:smembers(key(Category, p:app(State))),
  
  Objects = lists:map(fun(ObjectId) -> 
    Object = construct_object(ObjectId),
    bedrock_security:must_be_able_to_read(Object, State),
    Object 
  end, Ids),

  {ok, Objects, State}.

one(Category, State) ->
  bedrock_security:must_be_associated(State),

  Id = bedrock_redis:srandmember(key(Category, p:app(State))),

  Object = construct_object(ObjectId),
  bedrock_security:must_be_able_to_read(Object, State),

  {ok, Object, State}.

all_by_user(Id, State) ->
  bedrock_security:must_be_associated(State),

  UserKey = key(<<"objects">>, Id),
  Ids = bedrock_redis:smembers(UserKey)),
  
  Objects = lists:map(fun(ObjectId) -> 
    Object = construct_object(ObjectId),
    bedrock_security:must_be_able_to_read(Object, State),
    Object 
  end, Ids),

  {ok, Objects, State}.

all_by_user_with_category(Id, Category, State) ->
  bedrock_security:must_be_associated(State),

  CategoryKey = key(Category, p:app(State)),
  UserAndCatKey = key(CategoryKey, UserId),

  Ids = bedrock_redis:smembers(UserAndCatKey)),
  
  Objects = lists:map(fun(ObjectId) -> 
    Object = construct_object(ObjectId),
    bedrock_security:must_be_able_to_read(Object, State),
    Object 
  end, Ids),

  {ok, Objects, State}.

key(Field, Id) ->
  list_to_binary(io_lib:format("~s.~s", [Id, Field])).

construct_object(Id) ->
  Core = to_proplist(bedrock_redis:hgetall(key(<<"core">>, Id))),
  Attrs = to_proplist(bedrock_redis:hgetall(key(<<"attributes">>, Id))),
  [
    {<<"attributes">>, Attrs},
    {<<"readers">>, bedrock_redis:smembers(key(<<"readers">>))},
    {<<"writers">>, bedrock_redis:smembers(key(<<"writers">>))}
    |Core
  ].

destroy_object(Object) ->
  Id = p:id(Object),
  AppId = proplists:get_value(<<"application_id">>, Object),
  Category = proplists:get_value(<<"category">>, Object),

  Worker = bedrock_redis:start_transaction(),
  bedrock_redis:del(Worker, key(<<"core">>, Id)),
  bedrock_redis:del(Worker, key(<<"attributes">>, Id)),
  bedrock_redis:del(Worker, key(<<"readers">>, Id)),
  bedrock_redis:del(Worker, key(<<"writers">>, Id)),

  CategoryKey = key(Category, AppId),
  bedrock_redis:srem(Worker, CategoryKey, Id),
  
  case proplists:get_value(<<"user_id">>, Object) of
    undefined -> ok;
    UserId -> 
      UserKey = key(<<"objects">>, UserId),
      UserAndCatKey = key(CategoryKey, UserId),
      bedrock_redis:srem(Worker, UserKey, Id),
      bedrock_redis:srem(Worker, UserAndCatKey, Id)
  end,
  bedrock_redis:end_transaction(Worker).

to_proplist(List) ->
  [Key, Value|Rest] = List,
  [{Key, Value}]++to_proplist(Rest).
