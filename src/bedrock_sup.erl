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
  Dispatch = [
    {'_', [
        {'_', bedrock_handler, []}
    ]}
  ],

  Cowboy = cowboy:child_spec(bedrock_cowboy, 1024,
    cowboy_tcp_transport, [{port, 8080}],
    cowboy_http_protocol, [{dispatch, Dispatch}, {log, "bedrock_cowboy.log"}]
  ),

  {ok, { {one_for_one, 5, 10}, [Cowboy]} }.