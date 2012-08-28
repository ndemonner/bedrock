-module(bedrock_meter).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
       terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-export ([
  increase/3,
  decrease/3
]).

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

increase(ServiceId, Amount, State) ->
  poolboy:transaction(meter_pool, fun(Meter) -> 
    gen_server:cast(Meter, {increase, ServiceId, Amount, State})
  end).

decrease(ServiceId, Amount, State) ->
  poolboy:transaction(meter_pool, fun(Meter) -> 
    gen_server:cast(Meter, {decrease, ServiceId, Amount, State})
  end).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
  {ok, undefined}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({increase, ServiceId, Amount, HState}, State) ->
  % First we do the usage increment
  DUC = adjust(add, ServiceId, Amount, HState),
  % then check if need upgrade
  {noreply, State};

handle_cast({decrease, ServiceId, Amount, HState}, State) ->
  _DUC = adjust(sub, ServiceId, Amount, HState),
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

adjust(Mode, ServiceId, Amount, HState) ->
  DeveloperId  = proplists:get_value(developer_id, HState),
  Tables       = <<"developer_usage_constraints, usage_constraints">>,
  DevIdSql     = <<"developer_usage_constraints.developer_id = $1">>,
  ServIdSql    = <<"usage_constraints.service_id = $2">>,
  DUCSql       = <<"developer_usage_constraints.usage_constraint_id = usage_constraints.id">>,
  Where        = list_to_binary(io_lib:format("~s AND ~s AND ~s", [DevIdSql, ServIdSql, DUCSql])),
  Params       = [DeveloperId, ServiceId],
  {ok, [DUC]}  = bedrock_pg:find(Tables, Where, Params),
  DUCId        = proplists:get_value(<<"id">>, DUC),
  CurrentUsage = proplists:get_value(<<"usage">>, DUC),
  NewUsage     = case Mode of
    add -> CurrentUsage + Amount;
    sub -> CurrentUsage - Amount
  end,
  {ok, UpdatedDUC} = bedrock_pg:update(<<"developer_usage_constraints">>, DUCId, [{<<"usage">>, NewUsage}]),
  UpdatedDUC.

