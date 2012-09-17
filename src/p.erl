%%% Common proplists:get_values calls shortened for convenience.

-module (p).
-export ([
  id/1,
  identity/1
]).

id(Object) -> proplists:get_value(<<"id">>, Object).
identity(Object) -> proplists:get_value(identity, Object).