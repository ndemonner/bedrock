-module(bedrock_redis).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).
-export ([
  get/1, 
  set/2, 
  expire/2,
  delete/1
]).

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

get(Key) -> 
  poolboy:transaction(redis, fun(Worker) ->
    gen_server:call(Worker, {get, Key})
  end).

set(Key, Value) ->
  poolboy:transaction(redis, fun(Worker) ->
    gen_server:call(Worker, {set, Key, Value})
  end).

expire(Key, Time) ->
  poolboy:transaction(redis, fun(Worker) ->
    gen_server:call(Worker, {expire, Key, Time})
  end).

delete(Key) ->
  poolboy:transaction(redis, fun(Worker) ->
    gen_server:call(Worker, {delete, Key})
  end).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  Hostname = proplists:get_value(hostname, Args),
  {ok, Conn} = eredis:start_link(Hostname),
  {ok, [{connection, Conn}]}.

handle_call({get, Key}, _From, State) ->
  Connection = proplists:get_value(connection, State),
  {ok, Result} = eredis:q(Connection, ["GET", Key]),

  Result1 = try list_to_integer(binary_to_list(Result)) of
    Integer -> Integer
  catch
    _:_ -> Result
  end,

  {reply, Result1, State};

handle_call({set, Key, Value}, _From, State) ->
  Connection = proplists:get_value(connection, State),
  {ok, Result} = eredis:q(Connection, ["SET", Key, Value]),
  {reply, Result, State};

handle_call({expire, Key, Time}, _From, State) ->
  Connection = proplists:get_value(connection, State),
  {ok, Result} = eredis:q(Connection, ["EXPIRE", Key, Time]),
  {reply, Result, State};

handle_call({delete, Key}, _From, State) ->
  Connection = proplists:get_value(connection, State),
  {ok, Result} = eredis:q(Connection, ["DEL", Key]),
  {reply, Result, State};
  
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, State) ->
  Conn = proplists:get_value(connection, State),
  pgsql:close(Conn).

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------