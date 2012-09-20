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
  lrange/4,
  zadd/3,
  zadd/4,
  zremrangebyrank/3,
  zremrangebyrank/4,
  zrem/2,
  zrem/3,
  zrevrank/2,
  zrevrank/3,
  zscore/2,
  zscore/3,
  zincrby/3,
  zincrby/4,
  zcard/1,
  zcard/2,
  zrevrange/3,
  zrevrange/4
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
%% commands during a Redis transaction. If you aren't using a transaction, just use
%% the regular function without specifying a worker PID.

command(Args) ->
  poolboy:transaction(redis, fun(Worker) ->
    gen_server:call(Worker, {Args})
  end).

command(Worker, Args) ->
  gen_server:call(Worker, {Args}).

get(Key)                                    -> command(["GET", Key]).
get(Worker, Key)                            -> command(Worker, ["GET", Key]).

set(Key, Value)                             -> command(["SET", Key, Value]).
set(Worker, Key, Value)                     -> command(Worker, ["SET", Key, Value]).

expire(Key, Time)                           -> command(["EXPIRE", Key, Time]).
expire(Worker, Key, Time)                   -> command(Worker, ["EXPIRE", Key, Time]).

delete(Key)                                 -> command(["DEL", Key]).
delete(Worker, Key)                         -> command(Worker, ["DEL", Key]).

incr(Key)                                   -> command(["INCR", Key]).
incr(Worker, Key)                           -> command(Worker, ["INCR", Key]).

incrby(Key, Value)                          -> command(["INCRBY", Key, Value]).
incrby(Worker, Key, Value)                  -> command(Worker, ["INCRBY", Key, Value]).

decr(Key)                                   -> command(["DECR", Key])
decr(Worker, Key)                           -> command(Worker, ["DECR", Key]).

getset(Key, Value)                          -> command(["GETSET", Key, Value]).
getset(Worker, Key, Value)                  -> command(Worker, ["GETSET", Key, Value]).

lpush(Key, Value)                           -> command(["LPUSH", Key, Value]).
lpush(Worker, Key, Value)                   -> command(Worker, ["LPUSH", Key, Value]).

ltrim(Key, Start, Finish)                   -> command(["LTRIM", Key, Start, Finish]).
ltrim(Worker, Key, Start, Finish)           -> command(Worker, ["LTRIM", Key, Start, Finish]).

lrange(Key, Start, Finish)                  -> command(["LRANGE", Key, Start, Finish]).
lrange(Worker, Key, Start, Finish)          -> command(Worker, ["LRANGE", Key, Start, Finish]).

zadd(Set, Key, Score)                       -> command(["ZADD", Set, Key, Score]).
zadd(Worker, Set, Key, Score)               -> command(Worker, ["ZADD", Set, Key, Score]).

zremrangebyrank(Set, Start, Finish)         -> command(["ZREMRANGEBYRANK", Set, Start, Finish]).
zremrangebyrank(Worker, Set, Start, Finish) -> command(Worker, ["ZREMRANGEBYRANK", Set, Start, Finish]).

zrem(Set, Key)                              -> command(["ZREM", Set, Key]).
zrem(Worker, Set, Key)                      -> command(Worker, ["ZREM", Set, Key]).

zrevrank(Set, Key)                          -> command(["ZREVRANK", Set, Key]).
zrevrank(Worker, Set, Key)                  -> command(Worker, ["ZREVRANK", Set, Key]).

zscore(Set, Key)                            -> command(["ZSCORE", Set, Key]).
zscore(Worker, Set, Key)                    -> command(Worker, ["ZSCORE", Set, Key]).

zincrby(Set, Amount, Key)                   -> command(["ZINCRBY", Set, Amount, Key]).
zincrby(Worker, Set, Amount, Key)           -> command(Worker, ["ZINCRBY", Set, Amount, Key]).

zcard(Set)                                  -> command(["ZCARD", Set]).
zcard(Worker, Set)                          -> command(Worker, ["ZCARD", Set]).

zrevrange(Set, Start, Finish)               -> command(["ZREVRANGE", Set, Start, Finish]).
zrevrange(Worker, Set, Start, Finish)       -> command(Worker, ["ZREVRANGE", Set, Start, Finish]).

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

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  Hostname = proplists:get_value(hostname, Args),
  {ok, Conn} = eredis:start_link(Hostname),
  {ok, [{connection, Conn}]}.

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

handle_call({Args}, _From, State) ->
  Connection = proplists:get_value(connection, State),
  {ok, Result} = eredis:q(Connection, Args),
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