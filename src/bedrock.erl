-module (bedrock).
-export ([start/0, stop/0]).

start() ->
  application:start(crypto),
  application:start(bcrypt),
  application:start(hackney),
  application:start(bedrock).

stop() ->
  application:stop(bedrock),
  application:stop(hackney),
  application:stop(bcrypt),
  application:stop(crypto).