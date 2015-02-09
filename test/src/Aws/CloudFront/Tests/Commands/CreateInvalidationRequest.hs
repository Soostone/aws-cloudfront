{-# LANGUAGE OverloadedStrings #-}
module Aws.CloudFront.Tests.Commands.CreateInvalidationRequest
    ( tests
    ) where


-------------------------------------------------------------------------------
import           Control.Error
import           Data.List.NonEmpty                                (NonEmpty (..))
import           Data.Time.Calendar
import           Data.Time.Clock
import           Test.Tasty
import           Test.Tasty.HUnit
-------------------------------------------------------------------------------
import           Aws.CloudFront.Commands.CreateInvalidationRequest
import           Aws.CloudFront.TestHelpers
-------------------------------------------------------------------------------


tests :: TestTree
tests = testGroup "Aws.CloudFront.Commands.CreateInvalidationRequest"
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
    , invCreateTime      =  UTCTime d t
    }
  where
    d = fromGregorian 2009 11 19
    t = 19 * 60 * 60 + 37 * 60 + 58

