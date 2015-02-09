{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
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
    ) where


-------------------------------------------------------------------------------
import           Aws.Core
import           Aws.General
import           Data.List.NonEmpty  (NonEmpty)
import           Data.Monoid
import           Data.Text           (Text)
import           Data.Time
import           Data.Typeable
import           Text.XML.Cursor     (($/), ($//), (&/), (&//))
import qualified Text.XML.Cursor     as CU
-------------------------------------------------------------------------------
import           Aws.CloudFront.Core
-------------------------------------------------------------------------------


-- | Unique identifier for a batch
newtype CreateInvalidationRequestReference = CreateInvalidationRequestReference {
    createInvalidationRequestReferenceText :: Text
  } deriving (Show, Eq, Ord, Monoid, Typeable)

--TODO: smart constructor nonempty
--TODO: when do they specify the distribution id

-------------------------------------------------------------------------------
--TODO: smart constructor nonempty, leading slash, urlencode
newtype ObjectPath = ObjectPath {
      objectPathText :: Text
    } deriving (Show, Eq, Ord, Monoid, Typeable)


-------------------------------------------------------------------------------
data CreateInvalidationRequest = CreateInvalidationRequest {
      cirPaths           :: NonEmpty ObjectPath --TODO: use a stronger path type?
    , cirCallerReference :: CreateInvalidationRequestReference
    , cirDistributionId  :: DistributionId
    } deriving (Show, Eq, Ord, Typeable)

--TODO: returns an InvalidationResponse which is identical to the show response

instance SignQuery CreateInvalidationRequest

instance Transaction CreateInvalidationRequest CreateInvalidationResponse

-------------------------------------------------------------------------------
newtype CreateInvalidationResponse = CreateInvalidationResponse {
      ciresInvalidation :: Invalidation
    } deriving (Show, Eq, Ord, Typeable)

instance AsMemoryResponse CreateInvalidationResponse where
  type MemoryResponse CreateInvalidationResponse = CreateInvalidationResponse
  loadToMemory = return

instance ResponseConsumer r CreateInvalidationResponse where
  type ResponseMetadata CreateInvalidationResponse = CloudFrontMetadata
  responseConsumer _ = cloudFrontXmlResponseConsumer parse
    where
      --TODO: is there a monad transformer with throwM
      parse cursor = do
        cloudFrontCheckResponseType () "Invalidation" cursor
        let getContentOf n = cursor $/ CU.laxElement n &/ CU.content &/ fromText
        i <- getContentOf "Id"
        stat <- getContentOf "Status"
        ct <- getContentOf "CreateTime"
        batch <- force "Missing InvalidationBatch" $ cursor
                 $/ CU.laxElement "InvalidationBatch"
        cref <- batch $/ "CU.CallerReference"
                      &/ CU.content
        paths <- batch $/ CU.laxElement "Paths"
                       &/ CU.laxElement "Items"
                       &// CU.laxElement "Path"
                       &/ CU.content
        return $ Invalidation { invStatus = stat
                              , invPaths = paths
                              , invCallerReference = cref
                              , invInvalidationId = i
                              , invCreateTime = ct
                              }

-------------------------------------------------------------------------------
--todo: extract and share in a common types module
data Invalidation = Invalidation {
      invStatus          :: InvalidationStatus
    , invPaths           :: NonEmpty ObjectPath
    , invCallerReference :: CreateInvalidationRequestReference
    , invInvalidationId  :: InvalidationId
    , invCreateTime      :: UTCTime
    } deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
data InvalidationStatus = InvalidationInProgress
                        | InvalidationCompleted
                        deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
newtype InvalidationId = InvalidationId {
      invalidationIdText :: Text
    } deriving (Show, Eq, Ord, Typeable)
--TODO: error cases?
