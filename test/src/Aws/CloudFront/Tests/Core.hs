{-# LANGUAGE ScopedTypeVariables #-}
module Aws.CloudFront.Tests.Core
    ( tests
    ) where

-------------------------------------------------------------------------------
import           Test.Tasty
import           Test.Tasty.QuickCheck
-------------------------------------------------------------------------------
import           Aws.CloudFront
import           Aws.CloudFront.TestHelpers
-------------------------------------------------------------------------------


tests :: TestTree
tests = testGroup "Aws.CloudFront.Core"
  [ testProperty "AWSType CloudFrontAction" $ \(cfa :: CloudFrontAction) ->
     prop_AWSType_cycle cfa
  , testProperty "AwsType DistributionId" $ \ (x :: DistributionId) ->
     prop_AWSType_cycle x
  , testProperty "AwsType Marker" $ \ (x :: Marker) ->
     prop_AWSType_cycle x
  , testProperty "AwsType ObjectPath" $ \ (x :: ObjectPath) ->
     prop_AWSType_cycle x
  , testProperty "AwsType BucketName" $ \ (x :: BucketName) ->
     prop_AWSType_cycle x
  ]
