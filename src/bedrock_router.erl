-module(bedrock_router).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

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

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
  {ok, undefined, hibernate}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({handle, Msg, ConnectionState}, State) ->
  {ok, Unpacked} = msgpack:unpack(Msg),
  [Type|_] = Unpacked,
  case Type of
    0 -> 
      %% Req/rep call, so we get the unique id for the call,
      %% and after it's been routed and answered we pack it
      %% back up and send it on its way.

      bedrock_metrics:increment_counter_without_message(<<"_internal.counters.rpcs">>),
      Start = erlang:now(),

      [_,Id,_,_] = Unpacked,
      Response = route(rpc_to_proplist(request, Unpacked), ConnectionState),
      {_, _, RetState} = Response,

      NormalizedResponse = case Response of
        {ok, undefined, _} -> [{<<"ok">>, true},  {<<"body">>, <<"ok">>}];
        {ok, Body, _}      -> [{<<"ok">>, true},  {<<"body">>, maybe_wrap(Body)}];
        {error, Body, _}   -> [{<<"ok">>, false}, {<<"body">>, maybe_wrap(Body)}]
      end,

      {ok, Packed} = msgpack:pack([1, Id, {NormalizedResponse}]),

      Pid = proplists:get_value(pid, ConnectionState),
      Pid ! {send, Packed, RetState},

      Measured = timer:now_diff(erlang:now(), Start),
      bedrock_metrics:add_time_series_value_without_message(<<"_internal.series.responses">>, Measured / 1000);
    2 ->
      route(rpc_to_proplist(notify, Unpacked), ConnectionState)
  end,
  {noreply, State, hibernate};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(Info, State) ->
  lager:info("Info: ~p", Info),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

route(RPC, State) ->
  [{service, Module}, {method, Method}, {args, Args}] = RPC,
  Interface = list_to_atom("bedrock_"++Module++"_interface"),
  Function  = list_to_atom(Method),
  Params    = [maybe_proplist(Arg) || Arg <- Args]++[State],
  try erlang:apply(Interface, Function, Params) of
    Anything -> Anything
  catch
    throw:unauthorized       -> {error, unauthorized_message(), State};
    throw:unavailable        -> {error, unavailable_message(), State};
    throw:{requires_key, Id} -> {error, requires_key_message(Id), State};
    throw:{undefined, M}     -> {error, undefined_message(M), State};
    throw:{conflict, C}      -> {error, conflict_message(C), State};
    _:_                      -> {error, general_error_message(), State}
  end.
  % erlang:apply(Interface, Function, Params).

rpc_to_proplist(request, Message) ->
  [_Type, _Id, ServiceAndMethod, Args] = Message,
  [Service, Method] = string:tokens(binary_to_list(ServiceAndMethod), "."),
  [{service, Service}, {method, Method}, {args, Args}];

rpc_to_proplist(notify, Message) ->
  [_, ServiceAndMethod, Args] = Message,
  [Service, Method] = string:tokens(binary_to_list(ServiceAndMethod), "."),
  [{service, Service}, {method, Method}, {args, Args}].

general_error_message() ->
  <<"A general error was returned! Check to make sure you're calling the correct interface and method with the correct arguments. If this error persists, contact support.">>.

unauthorized_message() ->
  <<"You are not authorized to perform this call. The attempt has been logged.">>.

unavailable_message() ->
  <<"You have not added this add-on service to your account.">>.

requires_key_message(Id) ->
  {ok, Service} = bedrock_pg:get(<<"services">>, Id),
  Message = io_lib:format("The ~s service currently requires a valid test access key.", [proplists:get_value(<<"name">>, Service)]),
  list_to_binary(Message).

undefined_message(Undefined) ->
  Missing = string:join(Undefined, ", "),
  list_to_binary(io_lib:format("The following required fields are missing: ~s.", [Missing])).

conflict_message({K, V}) ->
  list_to_binary(io_lib:format("The ~s '~s' has already been used.", [K, V])).

maybe_proplist(Arg) ->
  case Arg of
    {[{_,_}|_]} -> {RealArg} = Arg, RealArg;
    {[]}        -> [];
    _           -> Arg
  end.

maybe_wrap([{_,_}|_] = Thing)  -> {[maybe_wrap(Tuple) || Tuple <- Thing]};
maybe_wrap([_|_] = List)       -> [maybe_wrap(Thing) || Thing <- List];
maybe_wrap({_,_} = Thing)      -> {maybe_wrap(element(1, Thing)), maybe_wrap(element(2, Thing))};
maybe_wrap(Thing)              -> Thing.
