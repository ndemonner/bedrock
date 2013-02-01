-module (bedrock).
-export ([start/0, stop/0]).

start() ->
  application:start(crypto),
  application:start(public_key),
  application:start(ssl),
  application:start(bcrypt),
  application:start(bedrock).

stop() ->
  application:stop(bedrock),
  application:stop(bcrypt),
  application:stop(ssl),
  application:stop(public_key),
  application:stop(crypto).