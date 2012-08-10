'use strict';

$(function() {
  // We want all p>spans to hide until they've been resolved
  var expr = $('.container-fluid>.row-fluid>div>p>span').html();
  var strippedExpr = expr.replace(/[\{\}]/g, '');
  $('.container-fluid>.row-fluid>div>p>span').attr('ng-cloak', '');
  $('.container-fluid>.row-fluid>div>p>span').attr('ng-show', strippedExpr);
});

// Now we boot up angular, but only after the websocket has connected to the server
angular.element(document).ready(function() {
  window.socket = new WebSocket('ws://localhost:8080');

  socket.binaryType = 'arraybuffer';

  socket.onopen = function() {
    console.log("Connected.");
    angular.bootstrap(document, ['btc']);
  };

  socket.onclose = function() {
    console.log("Disconnected.");
  };
});

// Declare app level module which depends on filters, and services
angular.module('btc', ['btc.services'])
  .config(function($locationProvider) {
    $locationProvider.html5Mode(false);
  })
  .run(function($rootScope, $log, bedrock) {
    $rootScope.deferredCounter = 0;
    $rootScope.deferreds = [];

    window.socket.onmessage = function(message) {
      bedrock.handle(message);
    };
  });