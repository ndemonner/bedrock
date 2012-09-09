-module(bedrock_redis).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).
-export ([
  get/1, 
  get/2, 
  set/2, 
  set/3, 
  expire/2,
  expire/3,
  delete/1,
  delete/2,
  incr/1,
  incr/2,
  incrby/2,
  incrby/3,
  getset/2,
  getset/3,
  decr/1,
  decr/2,
  publish/2,
  start_transaction/0,
  end_transaction/1,
  lpush/2,
  lpush/3,
  ltrim/3,
  ltrim/4,
  lrange/3,
  lrange/4
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

%% Most these functions have an alternate version where an explicit worker PID can
%% be specified. This is to ensure that the same worker builds a correct queue of
%% commands during a Redis transaction. If you aren't using a tranaction, just use
%% the regular function without specifying a worker PID.

get(Key) -> 
  poolboy:transaction(redis, fun(Worker) ->
    gen_server:call(Worker, {get, Key})
  end).
get(Worker, Key) -> 
  gen_server:call(Worker, {get, Key}).

set(Key, Value) ->
  poolboy:transaction(redis, fun(Worker) ->
    gen_server:call(Worker, {set, Key, Value})
  end).
set(Worker, Key, Value) ->
  gen_server:call(Worker, {set, Key, Value}).

expire(Key, Time) ->
  poolboy:transaction(redis, fun(Worker) ->
    gen_server:call(Worker, {expire, Key, Time})
  end).
expire(Worker, Key, Time) ->
  gen_server:call(Worker, {expire, Key, Time}).

delete(Key) ->
  poolboy:transaction(redis, fun(Worker) ->
    gen_server:call(Worker, {delete, Key})
  end).
delete(Worker, Key) ->
  gen_server:call(Worker, {delete, Key}).

incr(Key) ->
  poolboy:transaction(redis, fun(Worker) ->
    gen_server:call(Worker, {incr, Key})
  end).
incr(Worker, Key) ->
  gen_server:call(Worker, {incr, Key}).

incrby(Key, Value) ->
  poolboy:transaction(redis, fun(Worker) ->
    gen_server:call(Worker, {incrby, Key, Value})
  end).
incrby(Worker, Key, Value) ->
  gen_server:call(Worker, {incrby, Key, Value}).

decr(Key) ->
  poolboy:transaction(redis, fun(Worker) ->
    gen_server:call(Worker, {decr, Key})
  end).
decr(Worker, Key) ->
  gen_server:call(Worker, {decr, Key}).

publish(Channel, Message) ->
  poolboy:transaction(redis, fun(Worker) ->
    gen_server:call(Worker, {publish, Channel, Message})
  end).

start_transaction() ->
  Worker = poolboy:checkout(redis),
  gen_server:call(Worker, start_transaction).
  
end_transaction(Worker) ->
  Result = gen_server:call(Worker, end_transaction),
  poolboy:checkin(redis, Worker),
  Result.

getset(Key, Value) ->
  poolboy:transaction(redis, fun(Worker) ->
    gen_server:call(Worker, {getset, Key, Value})
  end).
getset(Worker, Key, Value) ->
  gen_server:call(Worker, {getset, Key, Value}).

lpush(Key, Value) ->
  poolboy:transaction(redis, fun(Worker) ->
    gen_server:call(Worker, {lpush, Key, Value})
  end).
lpush(Worker, Key, Value) ->
  gen_server:call(Worker, {lpush, Key, Value}).

ltrim(Key, Start, Finish) ->
  poolboy:transaction(redis, fun(Worker) ->
    gen_server:call(Worker, {ltrim, Key, Start, Finish})
  end).
ltrim(Worker, Key, Start, Finish) ->
  gen_server:call(Worker, {ltrim, Key, Start, Finish}).

lrange(Key, Start, Finish) ->
  poolboy:transaction(redis, fun(Worker) ->
    gen_server:call(Worker, {lrange, Key, Start, Finish})
  end).
lrange(Worker, Key, Start, Finish) ->
  gen_server:call(Worker, {lrange, Key, Start, Finish}).

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

handle_call({lpush, Key, Value}, _From, State) ->
  Connection = proplists:get_value(connection, State),
  {ok, Result} = eredis:q(Connection, ["LPUSH", Key, Value]),
  {reply, format(Result), State};

handle_call({ltrim, Key, Start, Finish}, _From, State) ->
  Connection = proplists:get_value(connection, State),
  {ok, Result} = eredis:q(Connection, ["LTRIM", Key, Start, Finish]),
  {reply, format(Result), State};

handle_call({lrange, Key, Start, Finish}, _From, State) ->
  Connection = proplists:get_value(connection, State),
  {ok, Result} = eredis:q(Connection, ["LRANGE", Key, Start, Finish]),
  {reply, format(Result), State};

handle_call({publish, Channel, Message}, _From, State) ->
  Connection = proplists:get_value(connection, State),
  {ok, Result} = eredis:q(Connection, ["PUBLISH", Channel, term_to_binary(Message)]),
  bedrock_metrics:increment_counter_without_message(<<"_internal.counters.messages">>),
  {reply, format(Result), State};

handle_call(start_transaction, _From, State) ->
  Connection = proplists:get_value(connection, State),
  eredis:q(Connection, ["MULTI"]),
  {reply, self(), State};

handle_call(end_transaction, _From, State) ->
  Connection = proplists:get_value(connection, State),
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

format(Result) when is_list(Result) ->
  [format(R) || R <- Result];

format(Result) ->
  try list_to_integer(binary_to_list(Result)) of
    Integer -> Integer
  catch
    _:_ -> Result
  end.