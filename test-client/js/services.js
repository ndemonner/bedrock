'use strict';

/* Services */

// Demonstrate how to register services
// In this case it is a simple value service.
angular.module('btc.services', [])
  .factory('bedrock', function($q, $rootScope) {
    return {
      rpc: function(method, args) {
        args = args || [];

        var deferred = $q.defer();
        // generate the id, pack the message, and then make the call
        var deferredId = $rootScope.deferredCounter++;

        var message = [0, deferredId, method, args];

        var packed = msgpack.encode(message);

        socket.send(packed);

        $rootScope.deferreds[deferredId] = deferred;
        return deferred.promise;
      },

      handle: function(message) {
        // unpack the message, and hook it up to the correct deferred object
        var unpacked = msgpack.decode(message.data);

        var deferredId = unpacked[1];

        var reply = unpacked[2];

        var deferred = $rootScope.deferreds[deferredId];

        $rootScope.$apply(function() {
          if (reply.ok) {
            deferred.resolve(reply.body);
          }
          else {
            deferred.reject(reply.body);
          }
        });
      }
    };
  });
