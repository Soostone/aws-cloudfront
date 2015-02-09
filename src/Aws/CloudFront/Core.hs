{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Aws.CloudFront.Core where


-------------------------------------------------------------------------------
import           Aws.Core
import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.ByteString        (ByteString)
import           Data.IORef
import           Data.Maybe
import           Data.Monoid
import           Data.Text              (Text, unpack)
import qualified Data.Text.Encoding     as T
import           Data.Typeable
import qualified Network.HTTP.Conduit   as HTTP
import qualified Network.HTTP.Types     as HTTP
import qualified Text.XML.Cursor        as CU
-------------------------------------------------------------------------------


newtype DistributionId = DistributionId {
      distributionIdText :: Text
    } deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
data CloudFrontMetadata = CloudFrontMetadata {
      cloudFrontMAmzId2    :: Maybe Text
    , cloudFrontMRequestId :: Maybe Text
    }

instance Loggable CloudFrontMetadata where
    toLogText (CloudFrontMetadata rid id2) =
        "CloudFront: request ID=" <> fromMaybe "<none>" rid
        <> ", x-amz-id-2=" <> fromMaybe "<none>" id2

instance Monoid CloudFrontMetadata where
    mempty = CloudFrontMetadata Nothing Nothing
    CloudFrontMetadata id1 r1 `mappend` CloudFrontMetadata id2 r2 = CloudFrontMetadata (id1 <|> id2) (r1 <|> r2)


-------------------------------------------------------------------------------
data CloudFrontConfiguration = CloudFrontConfiguration

-------------------------------------------------------------------------------
cloudFrontXmlResponseConsumer
    :: (CU.Cursor -> Response CloudFrontMetadata a)
    -> IORef CloudFrontMetadata
    -> HTTPResponseConsumer a
cloudFrontXmlResponseConsumer p metadataRef =
    cloudFrontResponseConsumer (xmlCursorConsumer p metadataRef) metadataRef


-------------------------------------------------------------------------------
cloudFrontResponseConsumer
    :: HTTPResponseConsumer a
    -> IORef CloudFrontMetadata
    -> HTTPResponseConsumer a
cloudFrontResponseConsumer inner metadata resp = do

    let headerString = fmap T.decodeUtf8 . flip lookup (HTTP.responseHeaders resp)
        amzId2 = headerString "x-amz-id-2"
        requestId = headerString "x-amz-request-id"
        m = CloudFrontMetadata { cloudFrontMAmzId2 = amzId2, cloudFrontMRequestId = requestId }

    liftIO $ tellMetadataRef metadata m

    if HTTP.responseStatus resp >= HTTP.status400
        then cloudFrontErrorResponseConsumer resp
        else inner resp


-------------------------------------------------------------------------------
cloudFrontErrorResponseConsumer = undefined


-------------------------------------------------------------------------------
cloudFrontCheckResponseType :: (MonadThrow m) => a -> Text -> CU.Cursor -> m a
cloudFrontCheckResponseType a n c = do
  _ <- force ("Expected response type " ++ unpack n) (CU.laxElement n c)
  return a


-------------------------------------------------------------------------------
data CloudFrontErrorResponse
    = CloudFrontErrorResponse
        { cloudFrontErrorStatusCode   :: !HTTP.Status
        , cloudFrontErrorCode         :: !Text
        , cloudFrontErrorMessage      :: !Text
        , cloudFrontErrorResource     :: !(Maybe Text)
        , cloudFrontErrorHostId       :: !(Maybe Text)
        , cloudFrontErrorAccessKeyId  :: !(Maybe Text)
        , cloudFrontErrorStringToSign :: !(Maybe ByteString)
        }
    | CloudFrontResponseDecodeError Text
    deriving (Show, Eq, Ord, Typeable)

instance Exception CloudFrontErrorResponse
