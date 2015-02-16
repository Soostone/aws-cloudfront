{-# LANGUAGE ScopedTypeVariables #-}
module Aws.CloudFront.Tests.Util
    ( tests
    ) where


-------------------------------------------------------------------------------
import           Test.Tasty
import           Test.Tasty.QuickCheck
-------------------------------------------------------------------------------
import           Aws.CloudFront.TestHelpers
import           Aws.CloudFront.Util
-------------------------------------------------------------------------------


tests :: TestTree
tests = testGroup "Aws.CloudFront.Util"
  [ testProperty "AwsType AWSUTCTime" $ \(autc :: AWSUTCTime) ->
     prop_AWSType_cycle autc
  , testProperty "AwsType AWSBool" $ \(autc :: AWSBool) ->
     prop_AWSType_cycle autc
  ]
