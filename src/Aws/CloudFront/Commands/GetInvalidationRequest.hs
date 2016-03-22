{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
module Aws.CloudFront.Commands.GetInvalidationRequest
    ( GetInvalidationRequest(..)
    , GetInvalidationResponse(..)
    ) where


-------------------------------------------------------------------------------
import           Aws.Core
import           Control.Error
import           Data.Monoid
import qualified Data.Text            as T
import           Data.Typeable
-------------------------------------------------------------------------------
import           Aws.CloudFront.Core
import           Aws.CloudFront.Types
-------------------------------------------------------------------------------


data GetInvalidationRequest = GetInvalidationRequest {
      girDistributionId :: !DistributionId
    , girInvalidationId :: !InvalidationId
    } deriving (Show, Eq, Ord, Typeable)


instance SignQuery GetInvalidationRequest where
  type ServiceConfiguration GetInvalidationRequest = CloudFrontConfiguration
  signQuery GetInvalidationRequest {..} = cloudFrontSignQuery CloudFrontQuery {
        cloudFrontQueryMethod = Get
      , cloudFrontQueryAction = GetInvalidation
      , cloudFrontQueryParameters = mempty
      , cloudFrontQueryBody = Nothing
      , cloudFrontQueryPathSegments = [ "distribution"
                                      , distributionIdText girDistributionId
                                      , "invalidation"
                                      , invalidationIdText girInvalidationId
                                      ]
      }


instance Transaction GetInvalidationRequest GetInvalidationResponse


-------------------------------------------------------------------------------
newtype GetInvalidationResponse = GetInvalidationResponse {
      giresInvalidation :: Invalidation
    } deriving (Show, Eq, Ord, Typeable)


instance AsMemoryResponse GetInvalidationResponse where
  type MemoryResponse GetInvalidationResponse = GetInvalidationResponse
  loadToMemory = return


instance ResponseConsumer r GetInvalidationResponse where
  type ResponseMetadata GetInvalidationResponse = CloudFrontMetadata
  responseConsumer _ = cloudFrontXmlResponseConsumer p
    where
      p cursor = do
        res <- runExceptT $ parseInvalidation cursor
        case res of
          Left e -> decodeError $ formatError e
          Right r -> return $ GetInvalidationResponse r
      formatError e = "Failed to parse cloudfront response: " <> (T.pack . show) e


--TODO: error cases?
