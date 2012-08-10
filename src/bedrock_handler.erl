-module(bedrock_handler).
-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
  websocket_info/3, websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_http_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
  {ok, Req, undefined_state}.

websocket_handle({binary, Msg}, Req, State) ->
  {ok, Unpacked} = msgpack:unpack(Msg),
  [Type|_] = Unpacked,
  case Type of
    0 -> 
      %% Req/rep call, so we get the unique id for the call,
      %% and after it's been routed and answered we pack it
      %% back up and send it on its way.
      [_,Id,_,_] = Unpacked,
      Reply = poolboy:transaction(router_pool, fun(Router) -> 
        gen_server:call(Router, {request, rpc_to_proplist(request, Unpacked)})
      end),

      NormalizedReply = case Reply of
        {ok, Body}    -> [{<<"ok">>, true},  {<<"body">>, Body}];
        {error, Body} -> [{<<"ok">>, false}, {<<"body">>, Body}]
      end,

      {ok, Packed} = msgpack:pack([1, Id, {NormalizedReply}]),
      {reply, {binary, Packed}, Req, State, hibernate};
    2 ->
      %% Fire and forget call, so don't send any reply. Do the call,
      %% and move on.
      poolboy:transaction(router_pool, fun(Router) -> 
        gen_server:cast(Router, {notify, rpc_to_proplist(notify, Unpacked)})
      end),
      {ok, Req, State, hibernate}
  end;

websocket_handle(_Data, Req, State) ->
  {ok, Req, State, hibernate}.

websocket_info(_Info, Req, State) ->
  {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.

rpc_to_proplist(request, Message) ->
  [_Type, _Id, ServiceAndMethod, Args] = Message,
  [Service, Method] = string:tokens(binary_to_list(ServiceAndMethod), "."),
  [{service, Service}, {method, Method}, {args, Args}];

rpc_to_proplist(notify, Message) ->
  [_, ServiceAndMethod, Args] = Message,
  [Service, Method] = string:tokens(binary_to_list(ServiceAndMethod), "."),
  [{service, Service}, {method, Method}, {args, Args}].