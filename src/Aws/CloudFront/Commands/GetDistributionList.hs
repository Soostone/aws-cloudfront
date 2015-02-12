{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
module Aws.CloudFront.Commands.GetDistributionList
    ( DistributionSummary(..)
    , DistributionStatus(..)
    , DomainName(..)
    , CName(..)
    , GetDistributionListRequest(..)
    , GetDistributionListResponse(..)
    , Origin(..)
    , CacheBehavior(..)
    , UnqualifiedCacheBehavior(..)
    , DistributionLogging(..)
    , GeoRestriction(..)
    , CountryCode
    , countryCodeText
    , mkCountryCode
    , PriceClass(..)
    , ViewerCertificate(..)
    , OriginId(..)
    , CustomErrorResponse(..)
    , S3OriginConfig(..)
    , CustomOriginConfig(..)
    , ForwardedValues(..)
    , CookieName(..)
    , CookieForwarding(..)
    , HeaderName(..)
    , MinTTL
    , minTTLSeconds
    , mkMinTTL
    , TrustedSignersSettings(..)
    , ViewerProtocolPolicy(..)
    , AllowedMethodGroup(..)
    , CachedMethodGroup(..)
    , PathPattern
    , pathPatternText
    , mkPathPattern
    , LoggingPrefix
    , loggingPrefixText
    , mkLoggingPrefix
    , ResponseCode(..)
    , ErrorCode(..)
    , HTTPPort
    , httpPortNumber
    , mkHTTPPort
    , OriginAccessIdentity(..)
    , OriginProtocolPolicy(..)
    , IAMCertificateId(..)
    , parseDistributionListResponse
    ) where


-------------------------------------------------------------------------------
import           Aws.Core
import           Aws.General
import           Control.Error
import           Control.Monad.Catch
import           Data.Char
import           Data.Ix
import           Data.List.NonEmpty  (NonEmpty (..))
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time
import           Data.Typeable
import qualified Text.XML.Cursor     as X
-------------------------------------------------------------------------------
import           Aws.CloudFront.Core
import           Aws.CloudFront.Util
-------------------------------------------------------------------------------


--TODO: i think some of this is actually DistributionConfig
--TODO: an XSD would make this a lot less ambiguous but all of them seem to have been expunged
data DistributionSummary = DistributionSummary {
      dsId                   :: DistributionId
    , dsStatus               :: DistributionStatus
    , dsLastModifiedTime     :: UTCTime
    , dsDomainName           :: DomainName
    , dsAliases              :: [CName]
    , dsOrigins              :: [Origin]
    , dsDefaultCacheBehavior :: UnqualifiedCacheBehavior
    , dsCacheBehaviors       :: [CacheBehavior] -- TODO: nonempty?
    , dsCustomErrorResponses :: [CustomErrorResponse]
    , dsRestrictions         :: Maybe GeoRestriction
    , dsComment              :: Maybe Text -- blank means Nothing
    , dsLogging              :: DistributionLogging
    , dsViewerCertificate    :: ViewerCertificate
    , dsPriceClass           :: PriceClass -- is this used elsewhere?
    , dsEnabled              :: Bool
    } deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
data GetDistributionListRequest = GetDistributionListRequest {
      gdlreqMarker :: Maybe Marker
    } deriving (Show, Eq, Ord, Typeable)


instance SignQuery GetDistributionListRequest where
  type ServiceConfiguration GetDistributionListRequest = CloudFrontConfiguration
  signQuery _gdlr@GetDistributionListRequest {..} = undefined


instance Transaction GetDistributionListRequest GetDistributionListResponse

-------------------------------------------------------------------------------
data GetDistributionListResponse = GetDistributionListResponse {
      gdlresCurMarker   :: Maybe Marker
    , gdlresNextMarker  :: Maybe Marker
    , gdlresIsTruncated :: Bool
    , gdlresSummaries   :: [DistributionSummary]
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
        res <- runEitherT $ parseDistributionListResponse cursor
        case res of
          Left e -> throwM $ CloudFrontResponseDecodeError $ formatError e
          Right r -> return r
      formatError e = "Failed to parse cloudfront response: " <> (T.pack . show) e
      --TODO: probably extract


parseDistributionListResponse
    :: (Functor m, MonadThrow m)
    => X.Cursor
    -> EitherT Text m GetDistributionListResponse
parseDistributionListResponse = error "TODO: parseDistributionListResponse"


-------------------------------------------------------------------------------
data DistributionStatus = DistributionDeployed
                        | DistributionInProgress
                        deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
newtype DomainName = DomainName {
      domainNameText :: Text
    } deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
newtype CName = CName {
      cNameText :: Text
    } deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
data Origin = Origin {
      originId         :: OriginId
    , originDomainName :: DomainName
    , originPath       :: Maybe ObjectPath -- TODO: verify
    , originConfig     :: Either S3OriginConfig CustomOriginConfig
    } deriving (Show, Eq, Ord, Typeable)

-------------------------------------------------------------------------------
data UnqualifiedCacheBehavior = UnqualifiedCacheBehavior {
      cbTargetOriginId       :: OriginId
    , cbForwardedValues      :: ForwardedValues
    , cbTrustedSigners       :: TrustedSignersSettings
    , cbViewerProtocolPolicy :: ViewerProtocolPolicy
    , cbMinTTL               :: MinTTL
    , cbAllowedMethods       :: Maybe AllowedMethodGroup --TODO: is this actually a maybe?
    , cbCachedMethods        :: Maybe CachedMethodGroup
    , cbSmoothStreaming      :: Bool
    } deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
data CacheBehavior = CacheBehavior {
      cbPathPattern :: PathPattern
    , cbBehavior    :: UnqualifiedCacheBehavior
    } deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
-- | A matcher for selecting cache behavior satisfying the following properties:
--   * A-Z, a-z
--   * 0-9
--   * _ - . * $ / ~ " ' @ : +
--   * A * as a character in the string, specified as \*
--   * &, passed and returned as &amp;
newtype PathPattern = PathPattern {
      pathPatternText :: Text
    } deriving (Show, Eq, Ord, Typeable)


--TODO: quickcheck
mkPathPattern :: Text -> Maybe PathPattern
mkPathPattern t
  | whitelistText pathPatternWhitelist (scrub t) = Just $ PathPattern t
  | otherwise                            = Nothing
  where
    scrub = T.replace "&amp;" "&" . T.replace "\\*" "*"


pathPatternWhitelist :: String
pathPatternWhitelist =
  ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-.*$/~\"'@:+"

-------------------------------------------------------------------------------
data AllowedMethodGroup = AMGH
                        -- ^ GET and HEAD
                        | AMGHO
                        -- ^ GET, HEAD, and OPTIONS
                        | AMALL
                        -- ^ DELETE, GET, HEAD, OPTIONS, PATCH, POST, PUT
                        deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
data CachedMethodGroup = CMGH
                       -- ^ GET and HEAD
                       | CMGHO
                       -- ^ GET, HEAD, and OPTIONS
                       deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
data ForwardedValues = ForwardedValues {
      fvQueryString :: Bool
    , fvCookies     :: CookieForwarding
    , fvHeaders     :: [HeaderName]
    } deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
newtype HeaderName = HeaderName {
      headerNameText :: Text
    } deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
data CookieForwarding = ForwardAllCookies
                      | ForwardNoCookies
                      | CookieWhitelist [CookieName]
                      deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
newtype CookieName = CookieName {
      cookieNameText :: Text
    } deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
data TrustedSignersSettings = TrustedSignersEnabled [AccountId] -- TODO: Docs say account number. is this equivalent?
                            | TrustedSignersDisabled
                            deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
data ViewerProtocolPolicy = VPPAllowAll
                          | VPPRedirectToHTTPS
                          | VPPHTTPSOnly
                          deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
-- | Minimum number of seconds you want objects to stay in CloudFront
-- caches before CloudFront queries your origin for updates. Valid
-- values are 0 and ~3,153,600,000 (100 years). A bounded instance has
-- been provided for this.
newtype MinTTL = MinTTL {
      minTTLSeconds :: Int
    } deriving (Show, Eq, Ord, Typeable)

--TODO: quickcheck with bounded
mkMinTTL :: Int -> Maybe MinTTL
mkMinTTL n
  | inRange (minMinTTL, maxMinTTL) n = Just $ MinTTL n
  | otherwise                        = Nothing


minMinTTL :: Int
minMinTTL = 0


maxMinTTL :: Int
maxMinTTL = 3153600000


instance Bounded MinTTL where
  minBound = MinTTL minMinTTL
  maxBound = MinTTL maxMinTTL


-------------------------------------------------------------------------------
data DistributionLogging = DistributionLogging {
      dlEnabled        :: Bool --TODO: do other fields appear if disabled?
    , dlIncludeCookies :: Bool
    , dlBucket         :: BucketName
    , dlPrefix         :: LoggingPrefix
    } deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
--TODO: smart constructor no starting slash, max 256 chars
newtype LoggingPrefix = LoggingPrefix {
      loggingPrefixText :: Text
    } deriving (Show, Eq, Ord, Typeable)


mkLoggingPrefix :: Text -> Maybe LoggingPrefix
mkLoggingPrefix t
  | not (T.null t) && pfx /= "/" && T.length t <= 256 = Just $ LoggingPrefix t
  | otherwise                                         = Nothing
  where
    pfx = T.take 1 t


-------------------------------------------------------------------------------
data GeoRestriction = GeoBlacklist (NonEmpty CountryCode)
                    | GeoWhitelist (NonEmpty CountryCode)
                    | GeoNoRestriction
                    deriving (Show, Eq, Ord, Typeable) --TODO


-------------------------------------------------------------------------------
newtype CountryCode = CountryCode {
    countryCodeText :: Text
  } deriving (Show, Eq, Ord, Typeable)


mkCountryCode :: Text -> Maybe CountryCode
mkCountryCode t
  | T.length t == 2 && T.all isAlpha t = Just $ CountryCode $ T.toUpper t
  | otherwise                          = Nothing


-------------------------------------------------------------------------------
data PriceClass = PriceClassAll
                -- ^ Requests are routed to all CloudFront edge locations based entirely on latency.
                | PriceClass200
                -- ^ Requests are routed to more edge locations than with PriceClass_100 but not to all edge locations.
                | PriceClass100
                -- ^ Requests are routed to edge locations in the least-expensive CloudFront regions.
                deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
data ViewerCertificate = ViewerCertificate { --TODO: iamcid implies ssl support method, MPV is a maybe
      vcCertificateStrategy    :: ViewerCertificateStrategy
    , vcMinimumProtocolVersion :: Maybe MinimumProtocolVersion
    } deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
data ViewerCertificateStrategy = UseIAM IAMCertificateId SSLSupportMethod
                               | CloudFrontDefaultCertificate
                               deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
--TODO: find out constraints
newtype IAMCertificateId = IAMCertificateID {
      iAMCertificateIdText :: Text
    } deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
data SSLSupportMethod = VIP
                      -- ^ CloudFront uses dedicated IP addresses for your content and can respond to HTTPS requests from any viewer. However, you must request permission to use this feature, and you incur additional monthly charges.
                      | SniOnly
                      -- ^ CloudFront can only respond to HTTPS requests from viewers that support Server Name Indication (SNI).
                      deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
data MinimumProtocolVersion = MinSSLv3
                            | MinTLSv1
                            deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
data OriginId = OriginId {
      originIdText :: Text
    } deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
data CustomErrorResponse = CustomErrorResponse {
      cerErrorCode        :: ErrorCode
    , cerResponsePagePath :: Maybe ObjectPath --TODO: is objectpath generally limited to 4k chars or do we need a separate type
    , cerResponseCode     :: ResponseCode
    , cerMinTTL           :: Maybe MinTTL
    } deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
--TODO: doc these
data ResponseCode = RC200
                  | RC400
                  | RC403
                  | RC404
                  | RC405
                  | RC414
                  | RC500
                  | RC501
                  | RC502
                  | RC503
                  | RC504
                  deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
data ErrorCode = EC400
               | EC403
               | EC404
               | EC405
               | EC414
               | EC500
               | EC501
               | EC502
               | EC503
               | EC504
               deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
--TODO: prepends "origin-access-identity/cloudfront/"
newtype S3OriginConfig = S3OriginConfig {
      s3OriginAccessIdentity :: OriginAccessIdentity
    } deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
newtype OriginAccessIdentity = OriginAccessIdentity {
      originAccessIdentityText :: Text --TODO: this is different from OriginId right?
    } deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
data CustomOriginConfig = CustomOriginConfig {
      cocHTTPPort             :: Maybe HTTPPort
    -- ^ Defaults to 80
    , cocHTTPSPort            :: Maybe HTTPPort
    -- ^ Defaults to 443
    , cocOriginProtocolPolicy :: OriginProtocolPolicy
    } deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
data OriginProtocolPolicy = OPPHTTPOnly
                          -- ^ CloudFront should only use HTTP to access the origin
                          | OPPMatchViewer
                          -- ^ CloudFront uses the same protocol as the viewer's request
                          deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
-- | 80, 443, or 1024-65535 inclusive
newtype HTTPPort = HTTPPort {
      httpPortNumber :: Int
    } deriving (Show, Eq, Ord, Typeable)


mkHTTPPort :: Int -> Maybe HTTPPort
mkHTTPPort n
  | n == 80 || n == 443 || inRange (1024, 65535) n = Just $ HTTPPort n
  | otherwise = Nothing
