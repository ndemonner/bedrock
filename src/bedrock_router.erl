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
  {ok, undefined}.

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
      Pid ! {send, Packed, RetState};
    2 ->
      route(rpc_to_proplist(notify, Unpacked), ConnectionState)
  end,
  {noreply, State, hibernate};

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

route(RPC, State) ->
  [{service, Module}, {method, Method}, {args, Args}] = RPC,
  Interface = list_to_atom("bedrock_"++Module++"_interface"),
  Function  = list_to_atom(Method),
  Params    = [maybe_proplist(Arg) || Arg <- Args]++[State],
  try erlang:apply(Interface, Function, Params) of
    Anything -> Anything
  catch
    throw:unauthorized -> {error, unauthorized_message(), State};
    _:_                -> {error, bad_interface_message(), State}
  end.

rpc_to_proplist(request, Message) ->
  [_Type, _Id, ServiceAndMethod, Args] = Message,
  [Service, Method] = string:tokens(binary_to_list(ServiceAndMethod), "."),
  [{service, Service}, {method, Method}, {args, Args}];

rpc_to_proplist(notify, Message) ->
  [_, ServiceAndMethod, Args] = Message,
  [Service, Method] = string:tokens(binary_to_list(ServiceAndMethod), "."),
  [{service, Service}, {method, Method}, {args, Args}].

bad_interface_message() ->
  <<"No such interface call! You may have supplied the wrong number of arguments.">>.

unauthorized_message() ->
  <<"You are not authorized to perform that call. The attempt has been logged.">>.

maybe_proplist(Arg) ->
  case Arg of
    {[{_,_}|_]} -> 
      {RealArg} = Arg,
      RealArg;
    _ -> Arg
  end.

maybe_wrap([{_,_}|_] = Thing)  -> {[maybe_wrap(Tuple) || Tuple <- Thing]};
maybe_wrap([_|_] = List)       -> [maybe_wrap(Thing) || Thing <- List];
maybe_wrap({_,_} = Thing)      -> {maybe_wrap(element(1, Thing)), maybe_wrap(element(2, Thing))};
maybe_wrap(Thing)              -> Thing.



