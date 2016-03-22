{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
module Aws.CloudFront.Commands.GetDistributionList
    ( GetDistributionListRequest(..)
    , GetDistributionListResponse(..)
    , parseDistributionListResponse
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


-------------------------------------------------------------------------------
data GetDistributionListRequest = GetDistributionListRequest {
      gdlreqMarker :: !(Maybe Marker)
    } deriving (Show, Eq, Ord, Typeable)


instance SignQuery GetDistributionListRequest where
  type ServiceConfiguration GetDistributionListRequest = CloudFrontConfiguration
  signQuery _gdlr@GetDistributionListRequest {..} = cloudFrontSignQuery CloudFrontQuery {
        cloudFrontQueryMethod = Get
      , cloudFrontQueryAction = GetDistributionList
      , cloudFrontQueryParameters = mempty
      , cloudFrontQueryBody = Nothing
      , cloudFrontQueryPathSegments = [ "distribution" ]
      }


instance Transaction GetDistributionListRequest GetDistributionListResponse

-------------------------------------------------------------------------------
data GetDistributionListResponse = GetDistributionListResponse {
      gdlresCurMarker   :: !(Maybe Marker)
    , gdlresNextMarker  :: !(Maybe Marker)
    , gdlresIsTruncated :: !Bool
    , gdlresSummaries   :: ![DistributionSummary]
    } deriving (Show, Eq, Ord, Typeable)


instance AsMemoryResponse GetDistributionListResponse where
  type MemoryResponse GetDistributionListResponse = GetDistributionListResponse
  loadToMemory = return


--TODO: extract this implemention to Core, parameterized on parser
instance ResponseConsumer r GetDistributionListResponse where
  type ResponseMetadata GetDistributionListResponse = CloudFrontMetadata
  responseConsumer _ = cloudFrontXmlResponseConsumer p
    where
      p cursor = do
        res <- runExceptT $ parseDistributionListResponse cursor
        case res of
          Left e -> decodeError $ formatError e
          Right r -> return r
      formatError e = "Failed to parse cloudfront response: " <> (T.pack . show) e
      --TODO: probably extract


-------------------------------------------------------------------------------
parseDistributionListResponse
    :: (Applicative m, MonadThrow m)
    => X.Cursor
    -> ExceptT Text m GetDistributionListResponse
parseDistributionListResponse cursor = do
  cloudFrontCheckResponseType () "DistributionList" cursor
  m <- ftMaybe $ cursor $/ le "Marker" &/ X.content
  nm <- ftMaybe $ cursor $/ le "NextMarker" &/ X.content
  it <- unAWSBool <$> getContentOf cursor "IsTruncated"
  dss <- mapM parseDistributionSummary $ cursor
         $/ le "Items"
         &/ le "DistributionSummary"
  return GetDistributionListResponse { gdlresCurMarker   = m
                                     , gdlresNextMarker  = nm
                                     , gdlresIsTruncated = it
                                     , gdlresSummaries   = dss
                                     }
