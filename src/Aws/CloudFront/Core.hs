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
import           Data.Conduit             (($$+-))
import           Data.IORef
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text                (Text)
import qualified Data.Text.Encoding       as T
import           Data.Typeable
import qualified Network.HTTP.Conduit     as HTTP
import qualified Network.HTTP.Types       as HTTP
import qualified Text.Parser.Char         as PC
import qualified Text.XML                 as X
import           Text.XML.Cursor          hiding (force)
-------------------------------------------------------------------------------
import           Aws.CloudFront.Types
-------------------------------------------------------------------------------


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
data CloudFrontConfiguration qt = CloudFrontConfiguration

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
    doc <- HTTP.responseBody resp $$+- X.sinkDoc X.def
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
cloudFrontSignQuery query _conf sigData = SignedQuery {
      sqMethod = method
    , sqProtocol = HTTPS
    , sqHost = host
    , sqPort = 443
    , sqPath = BB.toByteString $ HTTP.encodePathSegments path
    , sqQuery = HTTP.queryTextToQuery signedQuery
    , sqDate = Nothing
    , sqAuthorization = authorization
    , sqContentType = contentType
    , sqContentMd5 = Nothing
    , sqAmzHeaders = amzHeaders
    , sqOtherHeaders = [] -- headers -- we put everything into amzHeaders
    , sqBody = HTTP.RequestBodyBS <$> body
    , sqStringToSign = mempty -- Let me know if you really need this...
    }
  where
    -- values that don't depend on the signature
    action = cloudFrontQueryAction query
    path = apiVersion:(cloudFrontQueryPathSegments query)
    apiVersion = "2014-11-06"
    host = "cloudfront.amazonaws.com"
    headers = [("host", host)]
    contentType = case cloudFrontQueryMethod query of
        Post -> Just "application/xml"
        Get -> Nothing
        PostQuery -> Just "application/x-www-form-urlencoded; charset=utf-8"

        -- The following cases are currently not supported
        Put -> Just "application/xml"
        Delete -> Nothing
        Head -> Nothing

    method = case cloudFrontQueryMethod query of
        PostQuery -> Post
        x -> x

    body = case cloudFrontQueryMethod query of
        PostQuery -> Just $ BB.toByteString $ HTTP.renderQueryText False --TODO: probably scrap
            $ ("Action", Just . toText $ action) : cloudFrontQueryParameters query
        _ -> cloudFrontQueryBody query

    unsignedQuery = case cloudFrontQueryMethod query of
        PostQuery -> []
        _ -> ("Action", Just . toText $ action) : cloudFrontQueryParameters query

    -- Values that depend on the signature
    (signedQuery, amzHeaders, authorization) = case method of
        Get -> (getQuery, getAmzHeaders, getAuthorization)
        Head -> (getQuery, getAmzHeaders, getAuthorization)
        Delete -> (getQuery, getAmzHeaders, getAuthorization)
        Post -> (postQuery, postAmzHeaders, postAuthorization)
        PostQuery -> (postQuery, postAmzHeaders, postAuthorization)
        Put -> (postQuery, postAmzHeaders, postAuthorization)

    -- signatue dependend values for POST request
    postAmzHeaders = filter ((/= "Authorization") . fst) postSignature
    postAuthorization = return <$> lookup "authorization" postSignature
    postQuery = unsignedQuery
    postSignature = either error id $ signPostRequest
            (cred2cred $ signatureCredentials sigData)
            region
            ServiceNamespaceCloudfront
            (signatureTime sigData)
            (httpMethod method)
            path
            unsignedQuery
            headers
            (fromMaybe "" body)

    -- signature dependend values for GET request
    getAmzHeaders = headers
    getAuthorization = Nothing
    getQuery = getSignature
    getSignature = either error id $ signGetRequest
            (cred2cred $ signatureCredentials sigData)
            region
            ServiceNamespaceCloudfront
            (signatureTime sigData)
            (httpMethod method)
            path
            unsignedQuery
            headers
            (fromMaybe "" body)
    region = UsEast1 -- cloudfront is regionlist, so we have to supply us-east-1 apparently http://stackoverflow.com/questions/24603625/aws-cloudfront-credential-should-be-scoped-to-a-valid-region

#if MIN_VERSION_aws(0,9,2)
    cred2cred (Credentials a b c _) = SignatureV4Credentials a b c Nothing
#else
    cred2cred (Credentials a b c) = SignatureV4Credentials a b c Nothing
#endif


-------------------------------------------------------------------------------
data CloudFrontAction = CreateInvalidation
                      | GetInvalidation
                      | GetInvalidationList
                      | GetDistribution
                      | GetDistributionList
                      deriving (Show, Eq, Ord, Typeable)

--TODO: but why is this needed? i don't think this hits the cloudfront use case
--TODO: having an EOS would prevent us from having to shift precedence so much
instance AwsType CloudFrontAction where
  toText = fromString . show
  parse = (CreateInvalidation <$ PC.text "CreateInvalidation") <|>
          (GetInvalidationList <$ PC.text "GetInvalidationList") <|>
          (GetInvalidation <$ PC.text "GetInvalidation") <|>
          (GetDistributionList <$ PC.text "GetDistributionList") <|>
          (GetDistribution <$ PC.text "GetDistribution")
