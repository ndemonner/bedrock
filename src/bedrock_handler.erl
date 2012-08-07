-module(bedrock_handler).
-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
  websocket_info/3, websocket_terminate/3]).

init({tcp, http}, Req, Opts) ->
  {upgrade, protocol, cowboy_http_websocket}.

websocket_init(TransportName, Req, _Opts) ->
  {ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
  Reply = poolboy:transaction(router_pool, fun(Router) -> 
    gen_server:call(Router, {route, Msg});
  end),
  {reply, {text, Reply}, Req, State};

websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.