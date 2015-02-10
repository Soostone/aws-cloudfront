{-# LANGUAGE OverloadedStrings #-}
module Aws.CloudFront.Tests.Commands.CreateInvalidationRequest
    ( tests
    ) where


-------------------------------------------------------------------------------
import           Control.Error
import           Data.List.NonEmpty                                (NonEmpty (..))
import           Data.Text                                         (Text)
import qualified Data.Text                                         as T
import           Data.Time.Calendar
import           Data.Time.Clock
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Text.XML
import           Text.XML.Cursor

-- import Debug.Trace
-- import qualified Data.Text.Lazy as LT
-------------------------------------------------------------------------------
import           Aws.CloudFront.Commands.CreateInvalidationRequest
import           Aws.CloudFront.TestHelpers
-------------------------------------------------------------------------------


tests :: TestTree
tests = testGroup "Aws.CloudFront.Commands.CreateInvalidationRequest"
  [ parseInvalidationTests
  , renderCIRTests
  ]


-------------------------------------------------------------------------------
parseInvalidationTests :: TestTree
parseInvalidationTests = testGroup "parseInvalidation"
  [ testCase "it parses the example xml" $ do
      res <- runEitherT =<< parseFixture "Invalidation.xml" parseInvalidation
      res @?= Right expectedInvalidation
  ]


-------------------------------------------------------------------------------
renderCIRTests :: TestTree
renderCIRTests = testGroup "renderCIR"
  [ testProperty "never produces an empty path set" $ \req ->
     pathsCount (renderCIR req) > 0
  , testProperty "always produces a quantity > 1" $ \req ->
     pathsQuantity (renderCIR req) > 0
  , testProperty "always produces a quantity that is accurate" $ \req ->
     let res = renderCIR req
     in pathsQuantity res === pathsCount res
  , testProperty "always includes the caller reference" $ \req ->
     let res = renderCIR req
         expected = createInvalidationRequestReferenceText (cirCallerReference req)
     in getCallerRef res === [expected]
  ]



-------------------------------------------------------------------------------
getCallerRef :: Document -> [Text]
getCallerRef doc = c $/ laxElement "CallerReference" &/ content
  where
    c = fromDocument doc


-------------------------------------------------------------------------------
pathsCount :: Document -> Int
pathsCount doc = length $ c
                 $/ laxElement "Paths"
                 &/ laxElement "Items"
                 &/ laxElement "Path"
  where
    c = fromDocument doc


-------------------------------------------------------------------------------
pathsQuantity :: Document -> Int
pathsQuantity doc = read . T.unpack . fromMaybe "0" . listToMaybe $ c
                    $/ laxElement "Paths"
                    &/ laxElement "Quantity"
                    &/ content
  where
    c = fromDocument doc


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

