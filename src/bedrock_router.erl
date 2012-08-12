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

init(_Args) ->
  {ok, undefined}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({handle, Msg, Pid, ConnectionState}, State) ->
  {ok, Unpacked} = msgpack:unpack(Msg),
  [Type|_] = Unpacked,
  case Type of
    0 -> 
      %% Req/rep call, so we get the unique id for the call,
      %% and after it's been routed and answered we pack it
      %% back up and send it on its way.
      [_,Id,_,_] = Unpacked,
      Response = route(rpc_to_proplist(request, Unpacked), ConnectionState),
      {_, _, RetState} = Response,

      NormalizedResponse = case Response of
        {ok, _}          -> [{<<"ok">>, true},  {<<"body">>, <<"ok">>}];
        {ok, Body, _}    -> [{<<"ok">>, true},  {<<"body">>, Body}];
        {error, Body, _} -> [{<<"ok">>, false}, {<<"body">>, Body}]
      end,

      {ok, Packed} = msgpack:pack([1, Id, {NormalizedResponse}]),
      Pid ! {reply, Packed, RetState};
    2 ->
      route(rpc_to_proplist(notify, Unpacked), ConnectionState)
  end,
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

route(RPC, State) ->
  [{service, Module}, {method, Function}, {args, Args}] = RPC,
  erlang:apply(list_to_atom("bedrock_"++Module++"_interface"), list_to_atom(Function), Args++[State]).

rpc_to_proplist(request, Message) ->
  [_Type, _Id, ServiceAndMethod, Args] = Message,
  [Service, Method] = string:tokens(binary_to_list(ServiceAndMethod), "."),
  [{service, Service}, {method, Method}, {args, Args}];

rpc_to_proplist(notify, Message) ->
  [_, ServiceAndMethod, Args] = Message,
  [Service, Method] = string:tokens(binary_to_list(ServiceAndMethod), "."),
  [{service, Service}, {method, Method}, {args, Args}].
