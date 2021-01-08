{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Aws.CloudFront.Core where


-------------------------------------------------------------------------------
import           Aws.Core
import           Aws.General
import           Aws.SignatureV4
import qualified Blaze.ByteString.Builder as BB
import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import           Data.Conduit             (runConduit, (.|))
import           Data.IORef
import           Data.Maybe
import           Data.Monoid              as Monoid
import           Data.Semigroup           as Semigroup
import           Data.String
import           Data.Text                (Text)
import qualified Data.Text.Encoding       as T
import           Data.Typeable
import qualified Network.HTTP.Conduit     as HTTP
import qualified Network.HTTP.Types       as HTTP
import qualified Text.Parser.Char         as PC
import           Text.Parser.Combinators  ((<?>))
import qualified Text.Parser.Combinators  as PC
import qualified Text.XML                 as X
import           Text.XML.Cursor          hiding (force)
-------------------------------------------------------------------------------
import           Aws.CloudFront.Types
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
data CloudFrontMetadata = CloudFrontMetadata {
      cloudFrontMAmzId2    :: !(Maybe Text)
    , cloudFrontMRequestId :: !(Maybe Text)
    }

instance Loggable CloudFrontMetadata where
    toLogText (CloudFrontMetadata rid id2) =
        "CloudFront: request ID=" <> fromMaybe "<none>" rid
        <> ", x-amz-id-2=" <> fromMaybe "<none>" id2

instance Semigroup.Semigroup CloudFrontMetadata where
    CloudFrontMetadata id1 r1 <> CloudFrontMetadata id2 r2 = CloudFrontMetadata (id1 <|> id2) (r1 <|> r2)

instance Monoid.Monoid CloudFrontMetadata where
    mempty = CloudFrontMetadata Nothing Nothing
    mappend = (<>)


-------------------------------------------------------------------------------
data CloudFrontConfiguration qt = CloudFrontConfiguration {
      _cloudFrontConfiguration_host :: Text
    , _cloudFrontConfiguration_port :: Int
    , _cloudFrontConfiguration_protocol :: Protocol
    }


-------------------------------------------------------------------------------
defCloudFrontConfig :: CloudFrontConfiguration qt
defCloudFrontConfig =
    CloudFrontConfiguration "cloudfront.amazonaws.com" 443 HTTPS


-------------------------------------------------------------------------------
cloudFrontXmlResponseConsumer
    :: (Cursor -> Response CloudFrontMetadata a)
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
--TODO: verify this error format
cloudFrontErrorResponseConsumer :: HTTPResponseConsumer a
cloudFrontErrorResponseConsumer resp = do
    doc <- runConduit (HTTP.responseBody resp .| X.sinkDoc X.def)
    case parseError (fromDocument doc) of
        Right err -> liftIO $ throwM err
        Left otherErr -> do
            -- doc <- HTTP.responseBody resp $$+- consume
            -- liftIO $ print $ B8.concat $ doc
            liftIO $ throwM otherErr
  where
    parseError root = CloudFrontErrorResponse
        <$> pure (HTTP.responseStatus resp)
        <*> (force "Missing error Code" $ root $// elContent "Code")
        <*> (force "Missing error Message" $ root $// elContent "Message")
        <*> pure (listToMaybe $ root $// elContent "Resource")
        <*> pure (listToMaybe $ root $// elContent "HostId")
        <*> pure (listToMaybe $ root $// elContent "AWSAccessKeyId")
        <*> (pure $ do
            unprocessed <- listToMaybe $ root $// elCont "StringToSignBytes"
            B.pack <$> mapM readHex2 (words unprocessed))


-------------------------------------------------------------------------------
data CloudFrontQuery = CloudFrontQuery {
      cloudFrontQueryMethod       :: !Method
    , cloudFrontQueryAction       :: !CloudFrontAction
    , cloudFrontQueryParameters   :: !HTTP.QueryText
    , cloudFrontQueryBody         :: !(Maybe ByteString)
    , cloudFrontQueryPathSegments :: ![Text]
    }


-------------------------------------------------------------------------------
cloudFrontSignQuery
    :: CloudFrontQuery
    -> CloudFrontConfiguration qt
    -> SignatureData
    -> SignedQuery
cloudFrontSignQuery query cfConf sigData = SignedQuery {
      sqMethod = method
    , sqProtocol = _cloudFrontConfiguration_protocol cfConf
    , sqHost = host
    , sqPort = _cloudFrontConfiguration_port cfConf
    , sqPath = BB.toByteString $ HTTP.encodePathSegments path
    , sqQuery = HTTP.queryTextToQuery unsignedQuery
    , sqDate = Nothing
    , sqAuthorization = authorization
    , sqContentType = contentType
    , sqContentMd5 = Nothing
    , sqAmzHeaders = amzHeaders
    , sqOtherHeaders = [] -- headers -- we put everything into amzHeaders
    , sqBody = HTTP.RequestBodyBS <$> body
    , sqStringToSign = mempty
    }
  where
    action = cloudFrontQueryAction query
    path = apiVersion:(cloudFrontQueryPathSegments query)
    apiVersion = "2014-11-06"
    host = T.encodeUtf8 $ _cloudFrontConfiguration_host cfConf
    headers = [("host", host)]
    contentType = case cloudFrontQueryMethod query of
        Post      -> Just "application/xml"
        Get       -> Nothing
        PostQuery -> Just "application/x-www-form-urlencoded; charset=utf-8"

        -- The following cases are currently not supported
        Put       -> Just "application/xml"
        Delete    -> Nothing
        Head      -> Nothing

    method = case cloudFrontQueryMethod query of
        PostQuery -> Post
        x         -> x

    body = case cloudFrontQueryMethod query of
        PostQuery -> Just $ BB.toByteString $ HTTP.renderQueryText False --TODO: probably scrap
            $ ("Action", Just . toText $ action) : cloudFrontQueryParameters query
        _ -> cloudFrontQueryBody query

    unsignedQuery = case cloudFrontQueryMethod query of
        PostQuery -> []
        _ -> ("Action", Just . toText $ action) : cloudFrontQueryParameters query
    -- signatue dependend values for POST request
    amzHeaders = filter ((/= "Authorization") . fst) mkSignature
    authorization = return <$> lookup "authorization" mkSignature
    mkSignature = either error id $ signPostRequest
                  (cred2cred $ signatureCredentials sigData)
                  region
                  ServiceNamespaceCloudfront
                  (signatureTime sigData)
                  (httpMethod method)
                  path
                  unsignedQuery
                  headers
                  (fromMaybe "" body)
    -- cloudfront is regionlist, so we have to supply us-east-1
    -- apparently
    -- http://stackoverflow.com/questions/24603625/aws-cloudfront-credential-should-be-scoped-to-a-valid-region
    region = UsEast1

#if MIN_VERSION_aws(0,9,2)
    cred2cred (Credentials a b c _) = SignatureV4Credentials a b c Nothing
#else
    cred2cred (Credentials a b c)   = SignatureV4Credentials a b c Nothing
#endif


-------------------------------------------------------------------------------
data CloudFrontAction = CreateInvalidation
                      | GetInvalidation
                      | GetInvalidationList
                      | GetDistribution
                      | GetDistributionList
                      deriving (Show, Eq, Ord, Typeable)


instance AwsType CloudFrontAction where
  toText = fromString . show
  parse = ((CreateInvalidation <$ tok "CreateInvalidation")  <|>
          (GetInvalidationList <$ tok "GetInvalidationList") <|>
          (GetInvalidation <$ tok "GetInvalidation")         <|>
          (GetDistributionList <$ tok "GetDistributionList") <|>
          (GetDistribution <$ tok "GetDistribution")) <?> "parse CloudFrontAction"
    where
      tok t = PC.text t <* PC.eof
