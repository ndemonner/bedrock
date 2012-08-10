'use strict';

/* Controllers */

function InformationController($scope, $location, $log, bedrock) {
  $scope.version = bedrock.rpc('information.version');
};