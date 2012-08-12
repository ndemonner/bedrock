-module(bedrock_handler).
-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
  websocket_info/3, websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_http_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
  {ok, Req, [{authenticated, false}]}.

websocket_handle({binary, Msg}, Req, State) ->
  poolboy:transaction(router_pool, fun(Router) -> 
    gen_server:cast(Router, {handle, Msg, self(), State})
  end),
  {ok, Req, State, hibernate};

websocket_handle(_Data, Req, State) ->
  {ok, Req, State, hibernate}.

websocket_info({reply, Msg, NewState}, Req, _State) ->
  {reply, {binary, Msg}, Req, NewState, hibernate};

websocket_info(_Msg, Req, State) ->
  {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.