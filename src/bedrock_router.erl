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

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({handle, Msg, Pid}, State) ->
  {ok, Unpacked} = msgpack:unpack(Msg),
  [Type|_] = Unpacked,
  case Type of
    0 -> 
      %% Req/rep call, so we get the unique id for the call,
      %% and after it's been routed and answered we pack it
      %% back up and send it on its way.
      [_,Id,_,_] = Unpacked,
      Response = route(rpc_to_proplist(request, Unpacked)),

      NormalizedResponse = case Response of
        {ok, Body}    -> [{<<"ok">>, true},  {<<"body">>, Body}];
        {error, Body} -> [{<<"ok">>, false}, {<<"body">>, Body}]
      end,

      {ok, Packed} = msgpack:pack([1, Id, {NormalizedResponse}]),
      Pid ! {reply, Packed};
    2 ->
      route(rpc_to_proplist(notify, Unpacked))
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

route(RPC) ->
  [{service, Module}, {method, Function}, {args, Args}] = RPC,
  erlang:apply(list_to_atom("bedrock_"++Module++"_service"), list_to_atom(Function), Args).

rpc_to_proplist(request, Message) ->
  [_Type, _Id, ServiceAndMethod, Args] = Message,
  [Service, Method] = string:tokens(binary_to_list(ServiceAndMethod), "."),
  [{service, Service}, {method, Method}, {args, Args}];

rpc_to_proplist(notify, Message) ->
  [_, ServiceAndMethod, Args] = Message,
  [Service, Method] = string:tokens(binary_to_list(ServiceAndMethod), "."),
  [{service, Service}, {method, Method}, {args, Args}].
