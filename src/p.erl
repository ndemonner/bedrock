%%% Common proplists:get_values calls shortened for convenience.

-module (p).
-export ([
  id/1,
  identity/1,
  app/1,
  dev/1,
  format/2
]).

id(Object) -> proplists:get_value(<<"id">>, Object).
identity(Object) -> proplists:get_value(identity, Object).
app(Object) -> proplists:get_value(application, Object).
dev(Object) -> proplists:get_value(developer, Object).

format(Str, Args) ->
  list_to_binary(
    io_lib:format(Str, Args)
  ).