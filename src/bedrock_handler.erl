-module(bedrock_handler).
-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
  websocket_info/3, websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_http_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
  % create a dedicated redis client for pub/sub
  {ok, PubsubClient} = eredis_sub:start_link([{host, "redis.bedrock.io"}]),
  ok = eredis_sub:controlling_process(PubsubClient, self()),

  bedrock_redis:incr(<<"active-persons">>),
  bedrock_redis:publish(<<"person-connect">>, undefined),

  {ok, Req, [{pid, self()}, {pubsub_client, PubsubClient}]}.

websocket_handle({binary, Msg}, Req, State) ->
  bedrock_stats:rpc_made(),
  poolboy:transaction(router_pool, fun(Router) -> 
    gen_server:cast(Router, {handle, Msg, [{response_start_time, now()} | State]})
  end),
  {ok, Req, State, hibernate};

websocket_handle(_Data, Req, State) ->
  {ok, Req, State, hibernate}.

websocket_info({send, Msg, NewState}, Req, _State) ->
  {reply, {binary, Msg}, Req, bedrock_stats:store_response_time(NewState), hibernate};

websocket_info({message, Channel, Message, _Pid}, Req, State) ->
  PSClient = proplists:get_value(pubsub_client, State),
  eredis_sub:ack_message(PSClient),

  Term = binary_to_term(Message),

  {ok, Packed} = msgpack:pack([2, Channel, maybe_wrap(Term)]),
  {reply, {binary, Packed}, Req, State, hibernate};

websocket_info({subscribe, Channels}, Req, State) ->
  PSClient = proplists:get_value(pubsub_client, State),
  ok = eredis_sub:subscribe(PSClient, lists:flatten([Channels])),
  {ok, Req, State, hibernate};

websocket_info({unsubscribe, Channels}, Req, State) ->
  PSClient = proplists:get_value(pubsub_client, State),
  ok = eredis_sub:subscribe(PSClient, lists:flatten([Channels])),
  {ok, Req, State, hibernate};

websocket_info({subscribed, _Channel, _Pid}, Req, State) ->
  PSClient = proplists:get_value(pubsub_client, State),
  eredis_sub:ack_message(PSClient),
  {ok, Req, State, hibernate};

websocket_info(Msg, Req, State) ->
  lager:info(Msg),
  {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, State) ->
  bedrock_redis:decr(<<"active-persons">>),
  bedrock_redis:publish(<<"person-disconnect">>, undefined),

  eredis_sub:stop(proplists:get_value(pubsub_client, State)),
  ok.

maybe_wrap(undefined)          -> <<"undefined">>;
maybe_wrap([{_,_}|_] = Thing)  -> {[maybe_wrap(Tuple) || Tuple <- Thing]};
maybe_wrap([_|_] = List)       -> [maybe_wrap(Thing) || Thing <- List];
maybe_wrap({_,_} = Thing)      -> {maybe_wrap(element(1, Thing)), maybe_wrap(element(2, Thing))};
maybe_wrap(Thing)              -> Thing.
