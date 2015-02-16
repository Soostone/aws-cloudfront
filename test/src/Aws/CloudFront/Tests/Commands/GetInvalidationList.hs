{-# LANGUAGE OverloadedStrings #-}
module Aws.CloudFront.Tests.Commands.GetInvalidationList
    ( tests
    ) where


-------------------------------------------------------------------------------
import           Control.Error
import           Test.Tasty
import           Test.Tasty.HUnit
-------------------------------------------------------------------------------
import           Aws.CloudFront
import           Aws.CloudFront.TestHelpers
-------------------------------------------------------------------------------


tests :: TestTree
tests = testGroup "Aws.CloudFront.Commands.GetInvalidationList"
  [ parseInvalidationSummaryTests
  ]


-------------------------------------------------------------------------------
parseInvalidationSummaryTests :: TestTree
parseInvalidationSummaryTests = testGroup "parseInvalidationList"
  [ testCase "parses the example xml" $ do
      res <- runEitherT =<< parseFixture "ListInvalidations.xml" parseInvalidationListResponse
      res @?= Right expectedInvalidationListResponse
  ]


-------------------------------------------------------------------------------
expectedInvalidationListResponse :: GetInvalidationListResponse
expectedInvalidationListResponse = GetInvalidationListResponse {
      gilresCurMarker   = Just $ Marker "EGTXBD79EXAMPLE"
    , gilresNextMarker  = Just $ Marker "Invalidation ID"
    , gilresIsTruncated = True
    , gilresSummaries   = [sum1, sum2]
    }
  where
    sum1 = InvalidationSummary { isId     = InvalidationId "Second Invalidation ID"
                               , isStatus = InvalidationCompleted
                               }
    sum2 = InvalidationSummary { isId     = InvalidationId "First Invalidation ID"
                               , isStatus = InvalidationCompleted
                               }
