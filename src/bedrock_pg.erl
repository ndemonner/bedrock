-module(bedrock_pg).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).
-export ([
  get/2, 
  get_all/1, 
  get_all/2,
  find/3,
  find/4,
  insert/2, 
  update/3, 
  delete/2,
  delete_all/1,
  count/1,
  count_where/3
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

get(Table, Id) -> 
  poolboy:transaction(pg, fun(Worker) ->
    gen_server:call(Worker, {get, Table, Id})
  end).

get_all(Table) -> 
  poolboy:transaction(pg, fun(Worker) ->
    gen_server:call(Worker, {get_all, Table})
  end).

get_all(Table, Conditions) -> 
  poolboy:transaction(pg, fun(Worker) ->
    gen_server:call(Worker, {get_all, Table, Conditions})
  end).

find(Table, Where, Params) -> 
  poolboy:transaction(pg, fun(Worker) ->
    gen_server:call(Worker, {find, Table, Where, Params})
  end).

find(Table, Where, Params, Conditions) -> 
  poolboy:transaction(pg, fun(Worker) ->
    gen_server:call(Worker, {find, Table, Where, Params, Conditions})
  end).

insert(Table, Row) -> 
  poolboy:transaction(pg, fun(Worker) ->
    gen_server:call(Worker, {insert, Table, Row})
  end).

update(Table, Id, Changes) -> 
  poolboy:transaction(pg, fun(Worker) ->
    gen_server:call(Worker, {update, Table, Id, Changes})
  end).

delete(Table, Id) -> 
  poolboy:transaction(pg, fun(Worker) ->
    gen_server:call(Worker, {delete, Table, Id})
  end).

delete_all(Table) -> 
  poolboy:transaction(pg, fun(Worker) ->
    gen_server:call(Worker, {delete_all, Table})
  end).

count(Table) -> 
  poolboy:transaction(pg, fun(Worker) ->
    gen_server:call(Worker, {count, Table})
  end).

count_where(Table, Where, Params) -> 
  poolboy:transaction(pg, fun(Worker) ->
    gen_server:call(Worker, {count_where, Table, Where, Params})
  end).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  Hostname = proplists:get_value(hostname, Args),
  Database = proplists:get_value(database, Args),
  {ok, Conn} = pgsql:connect(Hostname, [{database, Database}]),
  {ok, [{connection, Conn}]}.

handle_call({get, Table, Id}, _From, State) ->
  Connection              = proplists:get_value(connection, State),
  Statement               = io_lib:format("SELECT * FROM ~s WHERE id = $1", [Table]),
  case pgsql:equery(Connection, Statement, [Id]) of
    {ok, RawColumns, [Row]} -> 
      % Extract the column names into a list
      Columns = [Column || {column, Column, _, _, _, _} <- RawColumns],
      Values = tuple_to_list(Row),

      % Zip them together
      ZippedList = lists:zip(Columns, Values),
      Result     = finalize(ZippedList),
      {reply, {ok, Result}, State};

    {ok, _RawColumns, []} -> {reply, {error, notfound}, State}
  end;

handle_call({get_all, Table}, _From, State) ->
  Connection              = proplists:get_value(connection, State),
  Statement               = io_lib:format("SELECT * FROM ~s", [Table]),
  {ok, RC, RR} = pgsql:equery(Connection, Statement, []),

  Columns = [Column || {column, Column, _, _, _, _} <- RC],
  Rows = [tuple_to_list(Row) || Row <- RR],

  ZippedLists = [lists:zip(Columns, Row) || Row <- Rows],
  Results = [finalize(ZL) || ZL <- ZippedLists],

  {reply, {ok, Results}, State};

handle_call({get_all, Table, Conditions}, _From, State) ->
  Connection              = proplists:get_value(connection, State),
  Statement               = io_lib:format("SELECT * FROM ~s ~s", [Table, Conditions]),
  {ok, RC, RR} = pgsql:equery(Connection, Statement, []),

  Columns = [Column || {column, Column, _, _, _, _} <- RC],
  Rows = [tuple_to_list(Row) || Row <- RR],

  ZippedLists = [lists:zip(Columns, Row) || Row <- Rows],
  Results = [finalize(ZL) || ZL <- ZippedLists],

  {reply, {ok, Results}, State};

handle_call({find, Table, Where, Params}, _From, State) ->
  Connection = proplists:get_value(connection, State),
  Statement  = io_lib:format("SELECT * FROM ~s WHERE ~s", [Table, Where]),
  {ok, RC, RR} = pgsql:equery(Connection, Statement, Params),

  Columns = [Column || {column, Column, _, _, _, _} <- RC],
  Rows = [tuple_to_list(Row) || Row <- RR],

  ZippedLists = [lists:zip(Columns, Row) || Row <- Rows],
  Results = [finalize(ZL) || ZL <- ZippedLists],

  {reply, {ok, Results}, State};

handle_call({find, Table, Where, Params, Conditions}, _From, State) ->
  Connection = proplists:get_value(connection, State),
  Statement  = io_lib:format("SELECT * FROM ~s WHERE ~s ~s", [Table, Where, Conditions]),
  {ok, RC, RR} = pgsql:equery(Connection, Statement, Params),

  Columns = [Column || {column, Column, _, _, _, _} <- RC],
  Rows = [tuple_to_list(Row) || Row <- RR],

  ZippedLists = [lists:zip(Columns, Row) || Row <- Rows],
  Results = [finalize(ZL) || ZL <- ZippedLists],

  {reply, {ok, Results}, State};

handle_call({insert, Table, Row}, _From, State) ->
  Connection = proplists:get_value(connection, State),

  InsertCols = [binary_to_list(Col) || {Col, _} <- Row],
  InsertColsSql = string:join(InsertCols, ", "),

  InsertParams = [Val || {_, Val} <- Row],
  InsertVars = [io_lib:format("$~w", [Var]) || Var <- lists:seq(1, length(InsertParams))],
  InsertVarsSql = string:join(InsertVars, ", "),

  StatementFmt = "INSERT INTO ~s (~s) VALUES (~s) RETURNING *",
  Statement  = io_lib:format(StatementFmt, [Table, InsertColsSql, InsertVarsSql]),

  case pgsql:equery(Connection, Statement, InsertParams) of
    {ok, _, RC, RR} -> 
      Columns = [Column || {column, Column, _, _, _, _} <- RC],
      Rows = [tuple_to_list(R) || R <- RR],

      ZippedLists = [lists:zip(Columns, R) || R <- Rows],
      Results = [finalize(ZL) || ZL <- ZippedLists],

      % We only insert one row at a time
      [Result] = Results,

      {reply, {ok, Result}, State};
    {error, Error} ->
      {reply, {error, Error}, State}
  end;

handle_call({update, Table, Id, PreChanges}, _From, State) ->
  Connection = proplists:get_value(connection, State),

  Changes = [{<<"updated">>, calendar:now_to_universal_time(now())} | PreChanges],

  Cols = [binary_to_list(Col) || {Col, _} <- Changes],
  Params = [Val || {_, Val} <- Changes],

  Vars = [io_lib:format("$~w", [Var]) || Var <- lists:seq(1, length(Params))],
  ColVars = lists:zip(Cols, Vars),

  ColVarsSqlList = [io_lib:format("~s = ~s", [Col, Var]) || {Col, Var} <- ColVars],

  SetSql = string:join(ColVarsSqlList, ", "),

  StatementFmt = "UPDATE ~s SET ~s WHERE id = ~w RETURNING *",
  Statement  = io_lib:format(StatementFmt, [Table, SetSql, Id]),

  case pgsql:equery(Connection, Statement, Params) of
    {ok, _, RC, RR} -> 
      Columns = [Column || {column, Column, _, _, _, _} <- RC],
      Rows = [tuple_to_list(R) || R <- RR],

      ZippedLists = [lists:zip(Columns, R) || R <- Rows],
      Results = [finalize(ZL) || ZL <- ZippedLists],

      % We only insert one row at a time
      [Result] = Results,

      {reply, {ok, Result}, State};
    {error, Error} ->
      {reply, {error, Error}, State}
  end;

handle_call({delete, Table, Id}, _From, State) ->
  Connection   = proplists:get_value(connection, State),
  StatementFmt = "DELETE FROM ~s WHERE id = ~w",
  Statement    = io_lib:format(StatementFmt, [Table, Id]),
  {ok, _}      = pgsql:equery(Connection, Statement, []),
  {reply, ok, State};

handle_call({delete_all, Table}, _From, State) ->
  Connection   = proplists:get_value(connection, State),
  StatementFmt = "DELETE FROM ~s",
  Statement    = io_lib:format(StatementFmt, [Table]),
  {ok, _}      = pgsql:equery(Connection, Statement, []),
  {reply, ok, State};

handle_call({count, Table}, _From, State) ->
  Connection   = proplists:get_value(connection, State),
  StatementFmt = "SELECT count(*) FROM ~s",
  Statement    = io_lib:format(StatementFmt, [Table]),
  {ok, RC, RR} = pgsql:equery(Connection, Statement, []),

  Columns = [Column || {column, Column, _, _, _, _} <- RC],
  Rows = [tuple_to_list(Row) || Row <- RR],

  ZippedLists = [lists:zip(Columns, Row) || Row <- Rows],
  Results = [finalize(ZL) || ZL <- ZippedLists],

  [Result] = Results,
  {reply, {ok, proplists:get_value(<<"count">>, Result)}, State};

handle_call({count_where, Table, Where, Params}, _From, State) ->
  Connection   = proplists:get_value(connection, State),
  StatementFmt = "SELECT count(*) FROM ~s WHERE ~s",
  Statement    = io_lib:format(StatementFmt, [Table, Where]),
  {ok, RC, RR} = pgsql:equery(Connection, Statement, [Params]),

  Columns = [Column || {column, Column, _, _, _, _} <- RC],
  Rows = [tuple_to_list(Row) || Row <- RR],

  ZippedLists = [lists:zip(Columns, Row) || Row <- Rows],
  Results = [finalize(ZL) || ZL <- ZippedLists],

  [Result] = Results,
  {reply, {ok, proplists:get_value(<<"count">>, Result)}, State};

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

maybe_convert(Value) ->
  case Value of 
    {{Year,Month,Day},{Hour,Min,Sec}} -> 
      UTCFormat = "~w-~2.2.0w-~2.2.0wT~2.2.0w:~2.2.0w:~2.2.0wZ",
      UTC = lists:flatten(io_lib:format(UTCFormat, [Year, Month, Day, Hour, Min, trunc(Sec)])),
      list_to_binary(UTC);
    Other -> Other
  end.

finalize(ZippedList) ->
  [{Column, maybe_convert(Value)} || {Column, Value} <- ZippedList].
