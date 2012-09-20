-module(bedrock_meter).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).
-export ([increment/3, decrement/3]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
       terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

increment(ServiceName, Amount, AppId) ->
  poolboy:transaction(meter_pool, fun(Meter) -> 
    gen_server:cast(Meter, {increment, ServiceName, Amount, AppId})
  end).

decrement(ServiceName, Amount, AppId) ->
  poolboy:transaction(meter_pool, fun(Meter) -> 
    gen_server:cast(Meter, {decrement, ServiceName, Amount, AppId})
  end).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
  {ok, undefined, hibernate}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({increment, ServiceName, Amount, AppId}, State) ->
  {ok, [Service]} = bedrock_pg:find(<<"services">>, <<"name = $1">>, [ServiceName]),
  ServiceId = p:id(Service),

  {ok, Application} = bedrock_pg:get(<<"applications">>, AppId),
  DevId = proplists:get_value(<<"developer_id">>, Application),

  Counter = list_to_binary(io_lib:format("developer.usage.~w", [DevId])),
  bedrock_metrics:increment_counter(Counter, Amount),

  % Check to see if they've gone over their capacity for this subscription
  Where = <<"service_id = $1 AND developer_id = $2">>,
  Params = [ServiceId, DevId],
  {ok, [Sub]} = bedrock_pg:find(<<"subscriptions">>, Where, Params),
  ConstraintId = proplists:get_value(<<"constraint_id">>, Sub),
  {ok, Constraint} = bedrock_pg:get(<<"constraints">>, ConstraintId),

  Capacity = proplists:get_value(<<"capacity">>, Constraint),
  Usage = bedrock_metrics:get_counter_value(Counter),

  case Usage > Capacity of
    false -> ok;
    true  ->
      CurrentTier = proplists:get_value(<<"tier">>, Constraint),
      change_subscription(ServiceId, CurrentTier + 1, DevId)
  end,

  {noreply, State};

handle_cast({decrement, ServiceName, Amount, AppId}, State) ->
  ServiceName = atom_to_binary(ServiceName, utf8),
  {ok, [Service]} = bedrock_pg:find(<<"services">>, <<"name = $1">>, [ServiceName]),
  ServiceId = p:id(Service),

  {ok, Application} = bedrock_pg:get(<<"applications">>, AppId),
  DevId = proplists:get_value(<<"developer_id">>, Application),

  Counter = list_to_binary(io_lib:format("developer.~w.usage", [DevId])),
  bedrock_metrics:decrement_counter(Counter, Amount),

  {noreply, State};  

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

change_subscription(ServiceId, NewTier, DevId) ->
  {ok, [NewConstraint]} = bedrock_pg:find(<<"constraints">>, <<"service_id = $1 AND tier = $2">>, [ServiceId, NewTier]),
  NewCost               = proplists:get_value(<<"cost">>, NewConstraint),
  NewConstraintId       = p:id(NewConstraint),
  Where                 = <<"developer_id = $1 AND service_id = $2">>,
  Params                = [DevId, ServiceId],
  {ok, [Sub]}           = bedrock_pg:find(<<"subscriptions">>, Where, Params),
  SubId                 = p:id(Sub),
  Changes               = [{<<"constraint_id">>, NewConstraintId}, {<<"cost">>, NewCost}],
  {ok, UpdatedSub}      = bedrock_pg:update(<<"subscriptions">>, SubId, Changes),
  bedrock_redis:publish(<<"subscription-changed">>, UpdatedSub).
