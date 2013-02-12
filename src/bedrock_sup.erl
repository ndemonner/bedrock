-module(bedrock_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, Pools} = application:get_env(bedrock, pools),
    PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
        PoolArgs = [{name, {local, Name}},
                    {worker_module, proplists:get_value(module, WorkerArgs)}] ++ SizeArgs,
        poolboy:child_spec(Name, PoolArgs, WorkerArgs)
    end, Pools),

  Dispatch = [
    {'_', [
        {'_', bedrock_handler, []}
    ]}
  ],

  CowboyTCP = cowboy:child_spec(bedrock_cowboy_tcp, 100,
    cowboy_tcp_transport, [
      {port, 8080}
    ],
    cowboy_http_protocol, [{dispatch, Dispatch}, {log, "bedrock_cowboy.log"}]
  ),
  CowboyTLS = cowboy:child_spec(bedrock_cowboy_tls, 100,
    cowboy_ssl_transport, [
      {port, 8443}, 
      {certfile, "priv/ssl/cert.crt"},
      {cacertfile, "priv/ssl/cacert.crt"},
      {keyfile, "priv/ssl/key.pem"}
    ],
    cowboy_http_protocol, [{dispatch, Dispatch}, {log, "bedrock_cowboy.log"}]
  ),

  {ok, { {one_for_one, 5, 10}, [CowboyTCP, CowboyTLS|PoolSpecs]} }.
