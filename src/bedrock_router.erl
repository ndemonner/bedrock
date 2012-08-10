-module(bedrock_router).
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

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  {ok, Args}.

handle_call({request, RPC}, _From, State) ->
  Reply = route(RPC),
  {reply, Reply, State, hibernate};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({notify, RPC}, State) ->
  route(RPC),
  {noreply, State, hibernate};

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

route(RPC) ->
  [{service, Module}, {method, Function}, {args, Args}] = RPC,
  erlang:apply(list_to_atom("bedrock_"++Module++"_service"), list_to_atom(Function), Args).
