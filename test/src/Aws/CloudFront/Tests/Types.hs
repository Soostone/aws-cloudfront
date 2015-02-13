{-# LANGUAGE OverloadedStrings #-}
module Aws.CloudFront.Tests.Types
    ( tests
    ) where

-------------------------------------------------------------------------------
import           Control.Error
import           Data.List.NonEmpty           (NonEmpty (..))
import           Test.Tasty
import           Test.Tasty.HUnit
-------------------------------------------------------------------------------
import           Aws.CloudFront.Core
import           Aws.CloudFront.Types
import           Aws.CloudFront.TestHelpers
-------------------------------------------------------------------------------


tests :: TestTree
tests = testGroup "Aws.CloudFront.Types"
  [ parseInvalidationTests
  ]


-------------------------------------------------------------------------------
parseInvalidationTests :: TestTree
parseInvalidationTests = testGroup "parseInvalidation"
  [ testCase "it parses the example xml" $ do
      res <- runEitherT =<< parseFixture "Invalidation.xml" parseInvalidation
      res @?= Right expectedInvalidation
  ]



-------------------------------------------------------------------------------
expectedInvalidation :: Invalidation
expectedInvalidation = Invalidation {
      invStatus          = InvalidationInProgress
    , invPaths           = ObjectPath "/foo/bar.html" :| [ObjectPath "/foo/baz.html"]
    , invCallerReference = CreateInvalidationRequestReference "1234567890"
    , invInvalidationId  =  InvalidationId "123"
    , invCreateTime      = mkUTCTime 2009 11 19 19 37 58
    }
