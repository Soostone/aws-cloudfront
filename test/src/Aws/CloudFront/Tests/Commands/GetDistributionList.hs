{-# LANGUAGE OverloadedStrings #-}
module Aws.CloudFront.Tests.Commands.GetDistributionList
    ( tests
    ) where


-------------------------------------------------------------------------------
import           Control.Error
-- import           Data.List.NonEmpty                          (NonEmpty (..))
import           Test.Tasty
import           Test.Tasty.HUnit
-- import           Test.Tasty.QuickCheck
-------------------------------------------------------------------------------
import           Aws.CloudFront.Commands.GetDistributionList
import           Aws.CloudFront.TestHelpers
-------------------------------------------------------------------------------


tests :: TestTree
tests = testGroup "Aws.CloudFront.Commands.GetDistributionList"
  [ parseDistributionListResponseTests
  ]


-------------------------------------------------------------------------------
parseDistributionListResponseTests :: TestTree
parseDistributionListResponseTests = testGroup "parseDistributionListResponse"
  [ testCase "it parses the example xml" $ do
      res <- runEitherT =<< parseFixture "ListDistributions.xml" parseDistributionListResponse
      res @?= Right expectedDistributionListResponse
  ]


-------------------------------------------------------------------------------
expectedDistributionListResponse :: GetDistributionListResponse
expectedDistributionListResponse = error "TODO expectedDistributionListResponse"
