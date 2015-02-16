{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
module Aws.CloudFront.Commands.GetInvalidationList
    ( GetInvalidationListRequest(..)
    , GetInvalidationListResponse(..)
    , InvalidationSummary(..)
    -- * Exported for testing
    , parseInvalidationListResponse
    ) where

-------------------------------------------------------------------------------
import           Aws.Core
import           Control.Applicative
import           Control.Error
import           Control.Monad.Catch
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Typeable
import           Text.XML.Cursor      (($/), (&/))
import qualified Text.XML.Cursor      as X
-------------------------------------------------------------------------------
import           Aws.CloudFront.Core
import           Aws.CloudFront.Types
import           Aws.CloudFront.Util
-------------------------------------------------------------------------------


data GetInvalidationListRequest = GetInvalidationListRequest {
      gilreqMarker         :: Maybe Marker
    , gilreqDistributionId :: DistributionId
    } deriving (Show, Eq, Ord, Typeable)


instance SignQuery GetInvalidationListRequest where
  type ServiceConfiguration GetInvalidationListRequest = CloudFrontConfiguration
  signQuery _gilr@GetInvalidationListRequest {..} = cloudFrontSignQuery CloudFrontQuery {
        cloudFrontQueryMethod = Get
      , cloudFrontQueryAction = GetInvalidationList
      , cloudFrontQueryParameters = mempty
      , cloudFrontQueryBody = Nothing
      , cloudFrontQueryPathSegments = [ "distribution"
                                      , distributionIdText gilreqDistributionId
                                      , "invalidation"
                                      ]
      }


instance Transaction GetInvalidationListRequest GetInvalidationListResponse


-------------------------------------------------------------------------------
data GetInvalidationListResponse = GetInvalidationListResponse {
      gilresCurMarker   :: Maybe Marker
    , gilresNextMarker  :: Maybe Marker
    , gilresIsTruncated :: Bool
    , gilresSummaries   :: [InvalidationSummary]
    } deriving (Show, Eq, Ord, Typeable)


instance AsMemoryResponse GetInvalidationListResponse where
  type MemoryResponse GetInvalidationListResponse = GetInvalidationListResponse
  loadToMemory = return


--TODO: extract this implemention to Core, parameterized on parser
instance ResponseConsumer r GetInvalidationListResponse where
  type ResponseMetadata GetInvalidationListResponse = CloudFrontMetadata
  responseConsumer _ = cloudFrontXmlResponseConsumer p
    where
      p cursor = do
        res <- runEitherT $ parseInvalidationListResponse cursor
        case res of
          Left e -> decodeError $ formatError e
          Right r -> return r
      formatError e = "Failed to parse cloudfront response: " <> (T.pack . show) e


-------------------------------------------------------------------------------
parseInvalidationListResponse
    :: (Applicative m, MonadThrow m)
    => X.Cursor
    -> EitherT Text m GetInvalidationListResponse
parseInvalidationListResponse cursor = do
  cloudFrontCheckResponseType () "InvalidationList" cursor
  m <- ftMaybe $ cursor $/ le "Marker" &/ X.content
  nm <- ftMaybe $ cursor $/ le "NextMarker" &/ X.content
  it <- unAWSBool <$> getContentOf cursor "IsTruncated"
  dss <- mapM parseInvalidationSummary $ cursor
         $/ le "Items"
         &/ le "InvalidationSummary"
  return GetInvalidationListResponse { gilresCurMarker   = m
                                     , gilresNextMarker  = nm
                                     , gilresIsTruncated = it
                                     , gilresSummaries   = dss
                                     }


-------------------------------------------------------------------------------
parseInvalidationSummary
    :: (Applicative m, MonadThrow m)
    => X.Cursor
    -> EitherT Text m InvalidationSummary
parseInvalidationSummary cursor = do
    i   <- getContentOf cursor "Id"
    s   <- getContentOf cursor "Status"
    return InvalidationSummary {
      isId     = i
    , isStatus = s
    }



-------------------------------------------------------------------------------
data InvalidationSummary = InvalidationSummary {
      isId     :: InvalidationId
    , isStatus :: InvalidationStatus
    } deriving (Show, Eq, Ord, Typeable)
