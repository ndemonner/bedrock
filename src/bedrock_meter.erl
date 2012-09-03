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
  adjust/3
]).

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

adjust(ServiceId, Amount, State) ->
  poolboy:transaction(meter_pool, fun(Meter) -> 
    gen_server:cast(Meter, {adjust, ServiceId, Amount, State})
  end).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
  {ok, undefined}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({adjust, ServiceId, Amount, HState}, State) ->
  % First we do the usage increment
  DUC = adjust_internal(ServiceId, Amount, HState),
  % then check if need upgrade
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

adjust_internal(ServiceId, Amount, HState) ->
  DeveloperId  = proplists:get_value(developer_id, HState),
  Where = <<"developer_id = $1 AND service_id = $2">>,
  Params = [DeveloperId, ServiceId],
  {ok, [DUC]}  = bedrock_pg:find(<<"developer_usage_constraints">>, Where, Params),
  DUCId        = proplists:get_value(<<"id">>, DUC),
  CurrentUsage = proplists:get_value(<<"usage">>, DUC),
  NewUsage     = CurrentUsage + Amount,
  {ok, UpdatedDUC} = bedrock_pg:update(<<"developer_usage_constraints">>, DUCId, [{<<"usage">>, NewUsage}]),
  {ok, UpdatedDUC}.

