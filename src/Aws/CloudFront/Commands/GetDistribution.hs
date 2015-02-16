{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
module Aws.CloudFront.Commands.GetDistribution
    ( GetDistributionRequest(..)
    , GetDistributionResponse(..)
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


data GetDistributionRequest = GetDistributionRequest {
      gdrId :: DistributionId
    } deriving (Show, Eq, Ord, Typeable)


instance SignQuery GetDistributionRequest where
  type ServiceConfiguration GetDistributionRequest = CloudFrontConfiguration
  signQuery GetDistributionRequest {..} = cloudFrontSignQuery CloudFrontQuery {
        cloudFrontQueryMethod = Get
      , cloudFrontQueryAction = GetDistribution
      , cloudFrontQueryParameters = mempty
      , cloudFrontQueryBody = Nothing
      , cloudFrontQueryPathSegments = [ "distribution"
                                      , distributionIdText gdrId
                                      ]
      }


instance Transaction GetDistributionRequest GetDistributionResponse


-------------------------------------------------------------------------------
newtype GetDistributionResponse = GetDistributionResponse {
      gdrDistribution :: DistributionSummary
    } deriving (Show, Eq, Ord, Typeable)


instance AsMemoryResponse GetDistributionResponse where
  type MemoryResponse GetDistributionResponse = GetDistributionResponse
  loadToMemory = return


instance ResponseConsumer r GetDistributionResponse where
  type ResponseMetadata GetDistributionResponse = CloudFrontMetadata
  responseConsumer _ = cloudFrontXmlResponseConsumer p
    where
      p cursor = do
        res <- runEitherT $ parseDistributionSummary cursor
        case res of
          Left e -> decodeError $ formatError e
          Right r -> return $ GetDistributionResponse r
      formatError e = "Failed to parse cloudfront response: " <> (T.pack . show) e
