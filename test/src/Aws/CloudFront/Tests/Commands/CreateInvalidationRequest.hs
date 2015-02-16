{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Aws.CloudFront.Tests.Commands.CreateInvalidationRequest
    ( tests
    ) where


-------------------------------------------------------------------------------
import           Control.Error
import           Data.Text                                         (Text)
import qualified Data.Text                                         as T
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Text.XML
import           Text.XML.Cursor
-------------------------------------------------------------------------------
import           Aws.CloudFront
import           Aws.CloudFront.TestHelpers
-------------------------------------------------------------------------------


tests :: TestTree
tests = testGroup "Aws.CloudFront.Commands.CreateInvalidationRequest"
  [ renderCIRTests
  , testProperty "CreateInvalidationRequestReference AWSType" $ \(cirr :: CreateInvalidationRequestReference) ->
    prop_AWSType_cycle cirr
  , testProperty "ObjectPath AWSType" $ \(op :: ObjectPath) ->
    prop_AWSType_cycle op
  , testProperty "InvalidationStatus AWSType" $ \(is :: InvalidationStatus) ->
    prop_AWSType_cycle is
  , testProperty "InvalidationId AWSType" $ \(ii :: InvalidationId) ->
    prop_AWSType_cycle ii
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
