'use strict';

/* Controllers */

function InformationController($scope, $location, $log, bedrock) {
  $scope.versionAfterDelay = bedrock.rpc('information.version_after_delay', [3000]);
  $scope.version = bedrock.rpc('information.version');
};