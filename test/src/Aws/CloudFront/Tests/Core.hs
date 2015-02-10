{-# LANGUAGE ScopedTypeVariables #-}
module Aws.CloudFront.Tests.Core
    ( tests
    ) where

-------------------------------------------------------------------------------
import           Test.Tasty
import           Test.Tasty.QuickCheck
-------------------------------------------------------------------------------
import           Aws.CloudFront.Core
import           Aws.CloudFront.TestHelpers
-------------------------------------------------------------------------------


tests :: TestTree
tests = testGroup "Aws.CloudFront.Core"
  [ testProperty "CloudFrontAction AWSType" $ \(cfa :: CloudFrontAction) ->
     prop_AWSType_cycle cfa
  ]
