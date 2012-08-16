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
  delete/1,
  incr/1,
  incrby/2,
  getset/2,
  decr/1,
  publish/2,
  transaction/2 
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

incr(Key) ->
  poolboy:transaction(redis, fun(Worker) ->
    gen_server:call(Worker, {incr, Key})
  end).

incrby(Key, Value) ->
  poolboy:transaction(redis, fun(Worker) ->
    gen_server:call(Worker, {incrby, Key, Value})
  end).

decr(Key) ->
  poolboy:transaction(redis, fun(Worker) ->
    gen_server:call(Worker, {decr, Key})
  end).

publish(Channel, Message) ->
  poolboy:transaction(redis, fun(Worker) ->
    gen_server:call(Worker, {publish, Channel, Message})
  end).

transaction(Fun, Args) ->
  poolboy:transaction(redis, fun(Worker) ->
    gen_server:call(Worker, {transaction, Fun, Args})
  end).

getset(Key, Value) ->
  poolboy:transaction(redis, fun(Worker) ->
    gen_server:call(Worker, {getset, Key, Value})
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
  {reply, format(Result), State};

handle_call({set, Key, Value}, _From, State) ->
  Connection = proplists:get_value(connection, State),
  {ok, Result} = eredis:q(Connection, ["SET", Key, Value]),
  {reply, format(Result), State};

handle_call({getset, Key, Value}, _From, State) ->
  Connection = proplists:get_value(connection, State),
  {ok, Result} = eredis:q(Connection, ["GETSET", Key, Value]),
  {reply, format(Result), State};

handle_call({expire, Key, Time}, _From, State) ->
  Connection = proplists:get_value(connection, State),
  {ok, Result} = eredis:q(Connection, ["EXPIRE", Key, Time]),
  {reply, format(Result), State};

handle_call({delete, Key}, _From, State) ->
  Connection = proplists:get_value(connection, State),
  {ok, Result} = eredis:q(Connection, ["DEL", Key]),
  {reply, format(Result), State};

handle_call({incr, Key}, _From, State) ->
  Connection = proplists:get_value(connection, State),
  {ok, Result} = eredis:q(Connection, ["INCR", Key]),
  {reply, format(Result), State};

handle_call({incrby, Key, Value}, _From, State) ->
  Connection = proplists:get_value(connection, State),
  {ok, Result} = eredis:q(Connection, ["INCRBY", Key, Value]),
  {reply, format(Result), State};

handle_call({decr, Key}, _From, State) ->
  Connection = proplists:get_value(connection, State),
  {ok, Result} = eredis:q(Connection, ["DECR", Key]),
  {reply, format(Result), State};

handle_call({publish, Channel, Message}, _From, State) ->
  Connection = proplists:get_value(connection, State),
  {ok, Result} = eredis:q(Connection, ["PUBLISH", Channel, term_to_binary(Message)]),
  bedrock_stats:message_sent(),
  {reply, format(Result), State};

handle_call({transaction, Fun, Args}, _From, State) ->
  Connection = proplists:get_value(connection, State),
  eredis:q(Connection, ["MULTI"]),
  Fun(Args),
  {ok, Result} = eredis:q(Connection, ["EXEC"]),
  {reply, format(Result), State};
  
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

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

format(Result) ->
  try list_to_integer(binary_to_list(Result)) of
    Integer -> Integer
  catch
    _:_ -> Result
  end.