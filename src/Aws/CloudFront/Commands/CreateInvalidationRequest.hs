{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
-- |
-- Module: Aws.CloudFront.Commands.CreateInvalidationRequest
-- Copyright: Copyright © 2015 SooStone, Inc.
-- License: MIT
-- Maintainer: Michael Xavier <michael.xavier@soostone.com>
-- Stability: experimental
--
-- /API Version: 2014-11-06/
--
-- Create an invalidation batch request. This removes an object from
-- edge-server caches before the scheduled expiry.
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/CreateInvalidation.html>
module Aws.CloudFront.Commands.CreateInvalidationRequest
    ( CreateInvalidationRequest(..)
    , CreateInvalidationResponse(..)
    -- * Exported for testing
    , renderCIR
    ) where


-------------------------------------------------------------------------------
import           Aws.Core
import           Control.Applicative
import           Control.Error
import qualified Data.ByteString.Lazy as LB
import           Data.List.NonEmpty   (NonEmpty (..))
import qualified Data.List.NonEmpty   as NE
import           Data.Monoid
import qualified Data.Text            as T
import           Data.Typeable
import           Text.XML
-------------------------------------------------------------------------------
import           Aws.CloudFront.Core
import           Aws.CloudFront.Types
-------------------------------------------------------------------------------


data CreateInvalidationRequest = CreateInvalidationRequest {
      cirPaths           :: !(NonEmpty ObjectPath)
    , cirCallerReference :: !CreateInvalidationRequestReference
    , cirDistributionId  :: !DistributionId
    } deriving (Show, Eq, Ord, Typeable)

--TODO: returns an InvalidationResponse which is identical to the show response

instance SignQuery CreateInvalidationRequest where
  type ServiceConfiguration CreateInvalidationRequest = CloudFrontConfiguration
  signQuery cir@CreateInvalidationRequest {..} = cloudFrontSignQuery CloudFrontQuery {
        cloudFrontQueryMethod = Post
      , cloudFrontQueryAction = CreateInvalidation
      , cloudFrontQueryParameters = mempty
      , cloudFrontQueryBody = Just $ LB.toStrict $ renderLBS def doc
      , cloudFrontQueryPathSegments = [ "distribution"
                                      , distributionIdText cirDistributionId
                                      , "invalidation"
                                      ]
      }
    where
      doc = renderCIR cir


instance Transaction CreateInvalidationRequest CreateInvalidationResponse


-------------------------------------------------------------------------------
renderCIR :: CreateInvalidationRequest -> Document
renderCIR CreateInvalidationRequest {..} = Document noPrologue root mempty
  where
    noPrologue = Prologue mempty Nothing mempty
    tnode = NodeContent
    root = Element "{http://cloudfront.amazonaws.com/doc/2014-11-06/}InvalidationBatch"
           mempty
           [NodeElement ref, NodeElement paths]
    ref = Element "CallerReference" mempty [tnode $ createInvalidationRequestReferenceText cirCallerReference]
    paths = Element "Paths" mempty [NodeElement qty, NodeElement items]
    qty = Element "Quantity" mempty [tnode . T.pack . show . NE.length $ cirPaths]
    items = Element "Items" mempty $ NE.toList (NodeElement . mkPath <$> cirPaths)
    mkPath (ObjectPath p) = Element "Path" mempty [tnode p]


-------------------------------------------------------------------------------
newtype CreateInvalidationResponse = CreateInvalidationResponse {
      ciresInvalidation :: Invalidation
    } deriving (Show, Eq, Ord, Typeable)


instance AsMemoryResponse CreateInvalidationResponse where
  type MemoryResponse CreateInvalidationResponse = CreateInvalidationResponse
  loadToMemory = return


instance ResponseConsumer r CreateInvalidationResponse where
  type ResponseMetadata CreateInvalidationResponse = CloudFrontMetadata
  responseConsumer _ = cloudFrontXmlResponseConsumer p
    where
      p cursor = do
        res <- runEitherT $ parseInvalidation cursor
        case res of
          Left e -> decodeError $ formatError e
          Right r -> return $ CreateInvalidationResponse r
      formatError e = "Failed to parse cloudfront response: " <> (T.pack . show) e


--TODO: error cases?
