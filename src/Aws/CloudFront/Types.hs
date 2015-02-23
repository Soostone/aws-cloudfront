{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
-- | Shared types and XML parsers. XML parsers are included because
-- some need to circumvent smart constructors for data coming from
-- Amazon.
module Aws.CloudFront.Types
    ( -- * Types
      Invalidation(..)
    , InvalidationStatus(..)
    , InvalidationId(..)
    , CreateInvalidationRequestReference(..)
    , DistributionId(..)
    , DistributionSummary(..)
    , DistributionStatus(..)
    , DomainName(..)
    , CName(..)
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
    , decodeError
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
    , SSLSupportMethod(..)
    , MinimumProtocolVersion(..)
    , ViewerCertificateStrategy(..)
    , AccountNumber(..)
    , AllowedMethods(..)
    , BucketName
    , bucketNameText
    , mkBucketName
    , ObjectPath(..)
    , CloudFrontErrorResponse(..)
    , Marker(..)
    -- * XML Parsers
    , parseInvalidation
    , parseDistributionSummary
    , cloudFrontCheckResponseType
    ) where

-------------------------------------------------------------------------------
import           Aws.Core
import           Aws.General
import           Control.Applicative
import           Control.Error
import           Control.Monad.Catch
import           Data.ByteString         (ByteString)
import           Data.Char
import           Data.Ix
import           Data.List               (sort)
import           Data.List.NonEmpty      (NonEmpty (..))
import           Data.Monoid
import           Data.String
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Time
import           Data.Traversable
import           Data.Typeable
import qualified Network.HTTP.Types      as HTTP
import           Prelude                 hiding (mapM)
import qualified Text.Parser.Char        as PC
import           Text.Parser.Combinators ((<?>))
import qualified Text.Parser.Combinators as PC
import           Text.XML.Cursor         (($/), (&/))
import qualified Text.XML.Cursor         as X
-------------------------------------------------------------------------------
import           Aws.CloudFront.Util
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | General
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


decodeError :: (MonadThrow m) => Text -> m a
decodeError = throwM . CloudFrontResponseDecodeError


-------------------------------------------------------------------------------
ftNonEmpty
    :: (MonadThrow m, AwsType a)
    => X.Cursor
    -> Text
    -> EitherT Text m (NonEmpty a)
ftNonEmpty cursor n = do
  xs <- ftList cursor n
  case xs of
    (h:tl) -> return $ h :| tl
    _ -> decodeError $ "Expected at least 1 " <> n


-------------------------------------------------------------------------------
newtype DistributionId = DistributionId {
      distributionIdText :: Text
    } deriving (Show, Eq, Ord, Typeable)

instance AwsType DistributionId where
  toText = toTextText . distributionIdText
  parse = DistributionId <$> parseTextText


-------------------------------------------------------------------------------
newtype Marker = Marker {
      markerText :: Text
    } deriving (Show, Eq, Ord, Typeable)


instance AwsType Marker where
  toText = toTextText . markerText
  parse = Marker <$> parseTextText


-------------------------------------------------------------------------------
--TODO: smart constructor nonempty, leading slash, urlencode
newtype ObjectPath = ObjectPath {
      objectPathText :: Text
    } deriving (Show, Eq, Ord, Monoid, Typeable)

instance AwsType ObjectPath where
  toText = toTextText . objectPathText
  parse = ObjectPath <$> parseTextText


-------------------------------------------------------------------------------
--TODO: limit exports, smart constructor
newtype BucketName = BucketName {
      bucketNameText :: Text
    } deriving (Show, Eq, Ord, Typeable)

--TODO: limit to range 1-128
mkBucketName :: Text -> Maybe BucketName
mkBucketName t
  | inRange (1, 128) (T.length t) = Just $ BucketName t
  | otherwise                     = Nothing


instance AwsType BucketName where
  toText = toTextText . bucketNameText
  parse = BucketName <$> parseTextText


-------------------------------------------------------------------------------
-- | Invalidations
-------------------------------------------------------------------------------



-- | Unique identifier for a batch
newtype CreateInvalidationRequestReference = CreateInvalidationRequestReference {
    createInvalidationRequestReferenceText :: Text
  } deriving (Show, Eq, Ord, Monoid, Typeable)


instance AwsType CreateInvalidationRequestReference where
  toText = toTextText . createInvalidationRequestReferenceText
  parse = CreateInvalidationRequestReference <$> parseTextText

--TODO: smart constructor nonempty
--TODO: when do they specify the distribution id


-------------------------------------------------------------------------------
data Invalidation = Invalidation {
      invStatus          :: !InvalidationStatus
    , invPaths           :: !(NonEmpty ObjectPath)
    , invCallerReference :: !CreateInvalidationRequestReference
    , invInvalidationId  :: !InvalidationId
    , invCreateTime      :: !UTCTime
    } deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
data InvalidationStatus = InvalidationInProgress
                        | InvalidationCompleted
                        deriving (Show, Eq, Ord, Typeable)


instance AwsType InvalidationStatus where
  toText InvalidationInProgress = "InProgress"
  toText InvalidationCompleted = "Completed"
  parse = (parseInProgress <|> parseCompleted) <?> "parse InvalidationStatus"
    where
      parseInProgress = PC.text "InProgress" *> pure InvalidationInProgress
      parseCompleted = PC.text "Completed" *> pure InvalidationCompleted


-------------------------------------------------------------------------------
newtype InvalidationId = InvalidationId {
      invalidationIdText :: Text
    } deriving (Show, Eq, Ord, Typeable)


instance AwsType InvalidationId where
  toText = toTextText . invalidationIdText
  parse = InvalidationId <$> parseTextText


-------------------------------------------------------------------------------
parseInvalidation
    :: (Functor m, MonadThrow m)
    => X.Cursor
    -> EitherT Text m Invalidation
parseInvalidation cursor = do
  cloudFrontCheckResponseType () "Invalidation" cursor
  i <- getContentOf cursor "Id"
  stat <- getContentOf cursor "Status"
  ct <- unAWSUTCTime <$> getContentOf cursor "CreateTime"
  batch <- force "Missing InvalidationBatch" $ cursor
           $/ le "InvalidationBatch"
  cref <- getContentOf batch "CallerReference"
  paths <- right $ batch
           $/ le "Paths"
           &/ le "Items" --TODO: extract Items parsing
           &/ le "Path"
           &/ X.content
  pathsNE <- case paths of
    (x:xs) -> hoistEither (traverse fromText' $ x :| xs)
    _ -> throwT "Empty Paths tag"
  return Invalidation { invStatus = stat
                      , invPaths = pathsNE
                      , invCallerReference = cref
                      , invInvalidationId = i
                      , invCreateTime = ct
                      }

-------------------------------------------------------------------------------
cloudFrontCheckResponseType :: (MonadThrow m) => a -> Text -> X.Cursor -> m a
cloudFrontCheckResponseType a n c = do
  _ <- force ("Expected response type " ++ T.unpack n) (X.laxElement n c)
  return a


-------------------------------------------------------------------------------
-- | Distributions
-------------------------------------------------------------------------------


--TODO: i think some of this is actually DistributionConfig
--TODO: an XSD would make this a lot less ambiguous but all of them seem to have been expunged
data DistributionSummary = DistributionSummary {
      dsId                   :: !DistributionId
    , dsStatus               :: !DistributionStatus
    , dsLastModifiedTime     :: !UTCTime
    , dsDomainName           :: !DomainName
    , dsAliases              :: ![CName]
    , dsOrigins              :: ![Origin]
    , dsDefaultCacheBehavior :: !UnqualifiedCacheBehavior
    , dsCacheBehaviors       :: ![CacheBehavior] -- TODO: nonempty?
    , dsCustomErrorResponses :: ![CustomErrorResponse]
    , dsRestrictions         :: !(Maybe GeoRestriction)
    , dsComment              :: !(Maybe Text) -- blank means Nothing
    , dsLogging              :: !(Maybe DistributionLogging)
    , dsViewerCertificate    :: !ViewerCertificate
    , dsPriceClass           :: !PriceClass -- is this used elsewhere?
    , dsEnabled              :: !Bool
    } deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
data DistributionStatus = DistributionDeployed
                        | DistributionInProgress
                        deriving (Show, Eq, Ord, Typeable)


instance AwsType DistributionStatus where
  toText DistributionDeployed = "Deployed"
  toText DistributionInProgress = "InProgress"
  parse = ((DistributionDeployed <$ PC.text "Deployed") <|>
          (DistributionInProgress <$ PC.text "InProgress")) <?> "parse DistributionStatus"


-------------------------------------------------------------------------------
newtype DomainName = DomainName {
      domainNameText :: Text
    } deriving (Show, Eq, Ord, Typeable)


instance AwsType DomainName where
  toText = toTextText . domainNameText
  parse = DomainName <$> parseTextText

-------------------------------------------------------------------------------
newtype CName = CName {
      cNameText :: Text
    } deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
data Origin = Origin {
      originId         :: !OriginId
    , originDomainName :: !DomainName
    , originPath       :: !(Maybe ObjectPath) -- TODO: verify
    , originConfig     :: !(Either S3OriginConfig CustomOriginConfig)
    } deriving (Show, Eq, Ord, Typeable)

-------------------------------------------------------------------------------
data UnqualifiedCacheBehavior = UnqualifiedCacheBehavior {
      cbTargetOriginId       :: OriginId
    , cbForwardedValues      :: ForwardedValues
    , cbTrustedSigners       :: TrustedSignersSettings
    , cbViewerProtocolPolicy :: ViewerProtocolPolicy
    , cbMinTTL               :: MinTTL
    , cbAllowedMethods       :: Maybe AllowedMethods
    , cbSmoothStreaming      :: Bool
    } deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
data CacheBehavior = CacheBehavior {
      cbPathPattern :: !PathPattern
    , cbBehavior    :: !UnqualifiedCacheBehavior
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


instance AwsType PathPattern where
  toText = toTextText . pathPatternText
  parse = PathPattern <$> parseTextText


-------------------------------------------------------------------------------
data AllowedMethods = AllowedMethods {
      amsAllowedGroup :: !AllowedMethodGroup
    , amsCachedGroup  :: !CachedMethodGroup
    } deriving (Show, Eq, Ord, Typeable)


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
      fvQueryString :: !Bool
    , fvCookies     :: !CookieForwarding
    , fvHeaders     :: ![HeaderName]
    } deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
newtype HeaderName = HeaderName {
      headerNameText :: Text
    } deriving (Show, Eq, Ord, Typeable)


instance AwsType HeaderName where
  toText = toTextText . headerNameText
  parse = HeaderName <$> parseTextText


-------------------------------------------------------------------------------
data CookieForwarding = ForwardAllCookies
                      | ForwardNoCookies
                      | CookieWhitelist !(NonEmpty CookieName)
                      deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
newtype CookieName = CookieName {
      cookieNameText :: Text
    } deriving (Show, Eq, Ord, Typeable)


instance AwsType CookieName where
  toText = toTextText . cookieNameText
  parse = CookieName <$> parseTextText


-------------------------------------------------------------------------------
data TrustedSignersSettings = TrustedSignersEnabled (NonEmpty AccountNumber) -- TODO: Docs say account number. is this equivalent?
                            | TrustedSignersDisabled
                            deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
data AccountNumber = SelfAccountNumber
                   | OtherAccountNumber !AccountId
                   deriving (Show, Eq, Ord, Typeable)


instance AwsType AccountNumber where
  toText SelfAccountNumber = "self"
  toText (OtherAccountNumber i) = toText i
  parse = ((SelfAccountNumber <$ PC.text "self") <|>
          (OtherAccountNumber <$> parse)) <?> "parse AccountNumber"


-------------------------------------------------------------------------------
data ViewerProtocolPolicy = VPPAllowAll
                          | VPPRedirectToHTTPS
                          | VPPHTTPSOnly
                           deriving (Show, Eq, Ord, Typeable)


instance AwsType ViewerProtocolPolicy where
  toText VPPAllowAll        = "allow-all"
  toText VPPRedirectToHTTPS = "redirect-to-https"
  toText VPPHTTPSOnly       = "https-only"
  parse = ((VPPAllowAll <$ PC.text "allow-all") <|>
          (VPPRedirectToHTTPS <$ PC.text "redirect-to-https") <|>
          (VPPHTTPSOnly <$ PC.text "https-only")) <?> "parse ViewerProtocolPolicy"


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


instance AwsType MinTTL where
  toText (MinTTL n) = fromString $ show n
  parse = MinTTL <$> parseInt


-------------------------------------------------------------------------------
data DistributionLogging = DistributionLogging {
      dlEnabled        :: !Bool --TODO: do other fields appear if disabled?
    , dlIncludeCookies :: !Bool
    , dlBucket         :: !BucketName
    , dlPrefix         :: !LoggingPrefix
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


instance AwsType LoggingPrefix where
  toText = toTextText . loggingPrefixText
  parse = LoggingPrefix <$> parseTextText


-------------------------------------------------------------------------------
data GeoRestriction = GeoAllowAll -- is GeoRestriction really optional or is omission GeoAllowAll
                    | GeoWhitelist !(NonEmpty CountryCode)
                    | GeoAllowNone
                    deriving (Show, Eq, Ord, Typeable) --TODO


-------------------------------------------------------------------------------
newtype CountryCode = CountryCode {
    countryCodeText :: Text
  } deriving (Show, Eq, Ord, Typeable)


mkCountryCode :: Text -> Maybe CountryCode
mkCountryCode t
  | T.length t == 2 && T.all isAlpha t = Just $ CountryCode $ T.toUpper t
  | otherwise                          = Nothing


instance AwsType CountryCode where
  toText = toTextText . countryCodeText
  parse = CountryCode <$> parseTextText


-------------------------------------------------------------------------------
data PriceClass = PriceClassAll
                -- ^ Requests are routed to all CloudFront edge locations based entirely on latency.
                | PriceClass200
                -- ^ Requests are routed to more edge locations than with PriceClass_100 but not to all edge locations.
                | PriceClass100
                -- ^ Requests are routed to edge locations in the least-expensive CloudFront regions.
                deriving (Show, Eq, Ord, Typeable)


instance AwsType PriceClass where
  toText PriceClassAll = "PriceClass_All"
  toText PriceClass200 = "PriceClass_200"
  toText PriceClass100 = "PriceClass_100"
  parse = ((PriceClassAll <$ PC.text "PriceClass_All") <|>
          (PriceClass200 <$ PC.text "PriceClass_200") <|>
          (PriceClass100 <$ PC.text "PriceClass_100")) <?> "parse PriceClass"


-------------------------------------------------------------------------------
data ViewerCertificate = ViewerCertificate { --TODO: iamcid implies ssl support method, MPV is a maybe
      vcCertificateStrategy    :: !ViewerCertificateStrategy
    , vcMinimumProtocolVersion :: !(Maybe MinimumProtocolVersion)
    } deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
data ViewerCertificateStrategy = UseIAM !IAMCertificateId !SSLSupportMethod -- are we sure that using the default obviates SSL support method?
                               | CloudFrontDefaultCertificate
                               deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
--TODO: find out constraints
newtype IAMCertificateId = IAMCertificateId {
      iAMCertificateIdText :: Text
    } deriving (Show, Eq, Ord, Typeable)


instance AwsType IAMCertificateId where
  toText = toTextText . iAMCertificateIdText
  parse = IAMCertificateId <$> parseTextText


-------------------------------------------------------------------------------
data SSLSupportMethod = VIP
                      -- ^ CloudFront uses dedicated IP addresses for your content and can respond to HTTPS requests from any viewer. However, you must request permission to use this feature, and you incur additional monthly charges.
                      | SniOnly
                      -- ^ CloudFront can only respond to HTTPS requests from viewers that support Server Name Indication (SNI).
                      deriving (Show, Eq, Ord, Typeable)


instance AwsType SSLSupportMethod where
  toText VIP     = "vip"
  toText SniOnly = "sni-only"
  parse = ((VIP <$ PC.text "vip") <|>
          (SniOnly <$ PC.text "sni-only")) <?> "parse SSLSupportMethod"


-------------------------------------------------------------------------------
data MinimumProtocolVersion = MinSSLv3
                            | MinTLSv1
                            deriving (Show, Eq, Ord, Typeable)


instance AwsType MinimumProtocolVersion where
  toText MinSSLv3 = "SSLv3"
  toText MinTLSv1 = "TLSv1"
  parse = ((MinSSLv3 <$ PC.text "SSLv3") <|>
          (MinTLSv1 <$ PC.text "TLSv1")) <?> "parse MinimumProtocolVersion"


-------------------------------------------------------------------------------
data OriginId = OriginId {
      originIdText :: Text
    } deriving (Show, Eq, Ord, Typeable)


instance AwsType OriginId where
  toText = toTextText . originIdText
  parse = OriginId <$> parseTextText


-------------------------------------------------------------------------------
data CustomErrorResponse = CustomErrorResponse {
      cerErrorCode        :: !ErrorCode
    , cerResponsePagePath :: !(Maybe ObjectPath) --TODO: is objectpath generally limited to 4k chars or do we need a separate type
    , cerResponseCode     :: !ResponseCode
    , cerMinTTL           :: !(Maybe MinTTL)
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


instance AwsType ResponseCode where
  toText RC200 = "200"
  toText RC400 = "400"
  toText RC403 = "403"
  toText RC404 = "404"
  toText RC405 = "405"
  toText RC414 = "414"
  toText RC500 = "500"
  toText RC501 = "501"
  toText RC502 = "502"
  toText RC503 = "503"
  toText RC504 = "504"
  parse = ((RC200 <$ PC.text "200") <|>
          (RC400 <$ PC.text "400") <|>
          (RC403 <$ PC.text "403") <|>
          (RC404 <$ PC.text "404") <|>
          (RC405 <$ PC.text "405") <|>
          (RC414 <$ PC.text "414") <|>
          (RC500 <$ PC.text "500") <|>
          (RC501 <$ PC.text "501") <|>
          (RC502 <$ PC.text "502") <|>
          (RC503 <$ PC.text "503") <|>
          (RC504 <$ PC.text "504")) <?> "parse ResponseCode"


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

instance AwsType ErrorCode where
  toText EC400 = "400"
  toText EC403 = "403"
  toText EC404 = "404"
  toText EC405 = "405"
  toText EC414 = "414"
  toText EC500 = "500"
  toText EC501 = "501"
  toText EC502 = "502"
  toText EC503 = "503"
  toText EC504 = "504"
  parse = ((EC400 <$ PC.text "400") <|>
          (EC403 <$ PC.text "403") <|>
          (EC404 <$ PC.text "404") <|>
          (EC405 <$ PC.text "405") <|>
          (EC414 <$ PC.text "414") <|>
          (EC500 <$ PC.text "500") <|>
          (EC501 <$ PC.text "501") <|>
          (EC502 <$ PC.text "502") <|>
          (EC503 <$ PC.text "503") <|>
          (EC504 <$ PC.text "504")) <?> "parse ErrorCode"


-------------------------------------------------------------------------------
-- | When provided an access identity, say abcde, produces the
-- specially-formatted config string
-- origin-access-identity/cloudfront/abcde
newtype S3OriginConfig = S3OriginConfig {
      s3OriginAccessIdentity :: Maybe OriginAccessIdentity
    } deriving (Show, Eq, Ord, Typeable)


instance AwsType S3OriginConfig where
  toText (S3OriginConfig Nothing)    = ""
  toText (S3OriginConfig (Just oic)) = "origin-access-identity/cloudfront/" <> toText oic
  parse = (S3OriginConfig <$> (parsePresent <|> parseNotPresent)) <?> "parse S3OriginConfig"
    where
      parsePresent = PC.text "origin-access-identity/cloudfront/" *> (Just <$> parse)
      parseNotPresent = Nothing <$ PC.eof


-------------------------------------------------------------------------------
newtype OriginAccessIdentity = OriginAccessIdentity {
      originAccessIdentityText :: Text --TODO: this is different from OriginId right?
    } deriving (Show, Eq, Ord, Typeable)


instance AwsType OriginAccessIdentity where
  toText = toTextText . originAccessIdentityText
  parse = OriginAccessIdentity <$> parseTextText

-------------------------------------------------------------------------------
data CustomOriginConfig = CustomOriginConfig {
      cocHTTPPort             :: !(Maybe HTTPPort)
    -- ^ Defaults to 80
    , cocHTTPSPort            :: !(Maybe HTTPPort)
    -- ^ Defaults to 443
    , cocOriginProtocolPolicy :: !OriginProtocolPolicy
    } deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
data OriginProtocolPolicy = OPPHTTPOnly
                          -- ^ CloudFront should only use HTTP to access the origin
                          | OPPMatchViewer
                          -- ^ CloudFront uses the same protocol as the viewer's request
                          deriving (Show, Eq, Ord, Typeable)


instance AwsType OriginProtocolPolicy where
  toText OPPHTTPOnly    = "http-only"
  toText OPPMatchViewer = "match-viewer"
  parse = ((OPPHTTPOnly <$ PC.text "http-only") <|>
          (OPPMatchViewer <$ PC.text "match-viewer")) <?> "parse OriginProtocolPolicy"

-------------------------------------------------------------------------------
-- | 80, 443, or 1024-65535 inclusive
newtype HTTPPort = HTTPPort {
      httpPortNumber :: Int
    } deriving (Show, Eq, Ord, Typeable)


mkHTTPPort :: Int -> Maybe HTTPPort
mkHTTPPort n
  | n == 80 || n == 443 || inRange (1024, 65535) n = Just $ HTTPPort n
  | otherwise = Nothing


instance AwsType HTTPPort where
  toText (HTTPPort n) = fromString $ show n
  parse = (HTTPPort <$> parseInt) <?> "parse HTTPPort"


-------------------------------------------------------------------------------
parseDistributionSummary
    :: (Applicative m, MonadThrow m)
    => X.Cursor
    -> EitherT Text m DistributionSummary
parseDistributionSummary cursor = do
  i   <- getContentOf cursor "Id"
  s   <- getContentOf cursor "Status"
  lmt <- unAWSUTCTime <$>  getContentOf cursor "LastModifiedTime"
  dn   <- getContentOf cursor "DomainName"
  let as = CName <$> (cursor
                      $/ le "Aliases"
                      &/ le "Items"
                      &/ le "CNAME"
                      &/ X.content)
  os <- mapM parseOrigin $ cursor
        $/ le "Origins"
        &/ le "Items"
        &/ le "Origin"
  dcb <- parseFirst cursor "DefaultCacheBehavior" parseUnqualifiedCacheBehavior
  cbs <- mapM parseCacheBehavior $ cursor
         $/ le "CacheBehaviors"
         &/ le "Items"
         &/ le "CacheBehavior"
  cers <-  mapM parseCustomErrorResponse $ cursor
           $/ le "CustomErrorResponses"
           &/ le "Items"
           &/ le "CustomErrorResponse"
  rs <- fmap listToMaybe . mapM parseGeoRestriction $ cursor
         $/ le "Restrictions"
         &/ le "GeoRestriction"
  lg <- fmap listToMaybe . mapM parseDistributionLogging $ cursor
         $/ le "Logging"
  let cmnt = listToMaybe $ cursor $/ le "Comment" &/ X.content
  vc <- parseFirst cursor "ViewerCertificate" parseViewerCertificate
  pc <- getContentOf cursor "PriceClass"
  en <- unAWSBool <$> getContentOf cursor "Enabled"

  return DistributionSummary {
    dsId                   = i
  , dsStatus               = s
  , dsLastModifiedTime     = lmt
  , dsDomainName           = dn
  , dsAliases              = as
  , dsOrigins              = os
  , dsDefaultCacheBehavior = dcb
  , dsCacheBehaviors       = cbs
  , dsCustomErrorResponses = cers
  , dsRestrictions         = rs
  , dsComment              = cmnt
  , dsLogging              = lg
  , dsViewerCertificate    = vc
  , dsPriceClass           = pc
  , dsEnabled              = en
  }



-------------------------------------------------------------------------------
parseOrigin
    :: (Monad m, Functor m, MonadThrow m)
    => X.Cursor
    -> EitherT Text m Origin
parseOrigin cursor = do
  i <- getContentOf cursor "Id"
  dn <- getContentOf cursor "DomainName"
  op <- ftMaybe $ cursor $/ le "OriginPath" &/ X.content
  let s3 = listToMaybe $ cursor
           $/ le "S3OriginConfig"
  oc <- case s3 of
    Just c -> Left <$> parseS3OriginConfig c
    Nothing -> do
      coc <- force "Missing CustomOriginConfig" $ cursor $/ le "CustomOriginConfig"
      Right <$> parseCustomOriginConfig coc
  return Origin {
    originId         = i
  , originDomainName = dn
  , originPath       = op
  , originConfig     = oc
  }


-------------------------------------------------------------------------------
parseGeoRestriction
    :: (Functor m, MonadThrow m)
    => X.Cursor
    -> EitherT Text m GeoRestriction
parseGeoRestriction cursor = do
    ty <- force "Missing RestrictionType" $ cursor
          $/ le "RestrictionType"
          &/ X.content
    case ty of
      "whitelist" -> GeoWhitelist <$> parseCodes
      "all"       -> return GeoAllowAll
      "none"      -> return GeoAllowNone
      _           -> decodeError $ "Invalid RestrictionType " <> ty
  where
    --TODO: extract parsing NonEmpty
    parseCodes = ftNonEmpty cursor "Location"





-------------------------------------------------------------------------------
parseS3OriginConfig
    :: (Functor m, MonadThrow m)
    => X.Cursor
    -> EitherT Text m S3OriginConfig
parseS3OriginConfig cursor = getContentOf cursor "OriginAccessIdentity"


-------------------------------------------------------------------------------
parseCustomOriginConfig
    :: (Functor m, MonadThrow m)
    => X.Cursor
    -> EitherT Text m CustomOriginConfig
parseCustomOriginConfig cursor = do
  hp <- ftMaybe $ cursor $/ le "HTTPPort" &/ X.content
  hsp <- ftMaybe $ cursor $/ le "HTTPSPort" &/ X.content
  opp <- getContentOf cursor "OriginProtocolPolicy"
  return CustomOriginConfig {
    cocHTTPPort             = hp
  , cocHTTPSPort            = hsp
  , cocOriginProtocolPolicy = opp
  }


-------------------------------------------------------------------------------
parseUnqualifiedCacheBehavior
    :: (Functor m, MonadThrow m)
    => X.Cursor
    -> EitherT Text m UnqualifiedCacheBehavior
parseUnqualifiedCacheBehavior cursor = do
  oid <- getContentOf cursor "TargetOriginId"
  fvs <- parseFirst cursor "ForwardedValues" parseForwardedValues
  tss <- parseFirst cursor "TrustedSigners" parseTrustedSignersSettings
  vpp <- getContentOf cursor "ViewerProtocolPolicy"
  mtl <- getContentOf cursor "MinTTL"
  ams <- fmap listToMaybe . mapM parseAllowedMethods $ cursor
         $/ le "AllowedMethods"
  ss <- unAWSBool <$> getContentOf cursor "SmoothStreaming"
  return UnqualifiedCacheBehavior {
    cbTargetOriginId       = oid
  , cbForwardedValues      = fvs
  , cbTrustedSigners       = tss
  , cbViewerProtocolPolicy = vpp
  , cbMinTTL               = mtl
  , cbAllowedMethods       = ams
  , cbSmoothStreaming      = ss
  }


-------------------------------------------------------------------------------
parseTrustedSignersSettings
    :: (Functor m, MonadThrow m)
    => X.Cursor
    -> EitherT Text m TrustedSignersSettings
parseTrustedSignersSettings cursor = do
  e <- unAWSBool <$> getContentOf cursor "Enabled"
  if e
    then TrustedSignersEnabled <$> ftNonEmpty cursor "AwsAccountNumber"
    else return TrustedSignersDisabled



-------------------------------------------------------------------------------
parseAllowedMethods
    :: (Functor m, MonadThrow m)
    => X.Cursor
    -> EitherT Text m AllowedMethods
parseAllowedMethods cursor = do
  amg <- parseAllowedMethodGroup cursor
  cms <- force "Missing CachedMethods" $ cursor $/ le "CachedMethods"
  cmg <- parseCachedMethodGroup cms
  return AllowedMethods {
    amsAllowedGroup = amg
  , amsCachedGroup  = cmg
  }

-------------------------------------------------------------------------------
parseAllowedMethodGroup
    :: (Functor m, MonadThrow m)
    => X.Cursor
    -> EitherT Text m AllowedMethodGroup
parseAllowedMethodGroup cursor = do
  let ms = sort $ cursor
        $/ le "Items"
        &/ le "Method"
        &/ X.content
  case ms of
    ["GET", "HEAD"] -> return AMGH
    ["GET", "HEAD", "OPTIONS"] -> return AMGHO
    ["DELETE", "GET", "HEAD", "OPTIONS", "PATCH", "POST", "PUT"] -> return AMALL
    _ -> decodeError $ "Invalid allowed method combination: " <> T.unwords ms


-------------------------------------------------------------------------------
parseCachedMethodGroup
    :: (Functor m, MonadThrow m)
    => X.Cursor
    -> EitherT Text m CachedMethodGroup
parseCachedMethodGroup cursor = do
  let ms = sort $ cursor
        $/ le "Items"
        &/ le "Method"
        &/ X.content
  case ms of
    ["GET", "HEAD"] -> return CMGH
    ["GET", "HEAD", "OPTIONS"] -> return CMGHO
    _ -> decodeError $ "Invalid cached method combination: " <> T.unwords ms


-------------------------------------------------------------------------------
parseForwardedValues
    :: (Functor m, MonadThrow m)
    => X.Cursor
    -> EitherT Text m ForwardedValues
parseForwardedValues cursor = do
  qs      <- unAWSBool <$> getContentOf cursor "QueryString"
  cks     <- parseFirst cursor "Cookies" parseCookieFowarding
  headers <- force "Missing Headers" $ cursor
             $/ le "Headers"
  hs      <- ftList headers "Name"
  return ForwardedValues {
    fvQueryString = qs
  , fvCookies     = cks
  , fvHeaders     = hs
  }

-------------------------------------------------------------------------------
parseCookieFowarding
    :: (Functor m, MonadThrow m)
    => X.Cursor
    -> EitherT Text m CookieForwarding
parseCookieFowarding cursor = do
  fwd <- force "Missing Foward" $ cursor
         $/ le "Forward"
         &/ X.content
  case fwd of
    "whitelist" -> do
      names <- force "Missing WhitelistedNames" $ cursor
               $/ le "WhitelistedNames"
      CookieWhitelist <$> ftNonEmpty names "Name"
    "all"       -> return ForwardAllCookies
    "none"      -> return ForwardNoCookies
    _           -> decodeError $ "Invalid Forward " <> fwd


-------------------------------------------------------------------------------
parseCacheBehavior
    :: (Functor m, MonadThrow m)
    => X.Cursor
    -> EitherT Text m CacheBehavior
parseCacheBehavior cursor = do
  pp <- getContentOf cursor "PathPattern"
  cb <- parseUnqualifiedCacheBehavior cursor
  return CacheBehavior {
    cbPathPattern = pp
  , cbBehavior    = cb
  }


-------------------------------------------------------------------------------
parseCustomErrorResponse
    :: (Functor m, MonadThrow m)
    => X.Cursor
    -> EitherT Text m CustomErrorResponse
parseCustomErrorResponse cursor = do
  ec <- getContentOf cursor "ErrorCode"
  rpp <- ftMaybe $ cursor
         $/ le "ResponsePagePath"
         &/ X.content
  rc <- getContentOf cursor "ResponseCode"
  mtl <- ftMaybe $ cursor
         $/ le "ErrorCachingMinTTL"
         &/ X.content
  return CustomErrorResponse {
    cerErrorCode        = ec
  , cerResponsePagePath = rpp
  , cerResponseCode     = rc
  , cerMinTTL           = mtl
  }


-------------------------------------------------------------------------------
parseDistributionLogging
    :: (Functor m, MonadThrow m)
    => X.Cursor
    -> EitherT Text m DistributionLogging
parseDistributionLogging cursor = do
  en <- unAWSBool <$> getContentOf cursor "Enabled"
  ic <- unAWSBool <$> getContentOf cursor "IncludeCookies"
  b <- getContentOf cursor "Bucket"
  lp <- getContentOf cursor "Prefix"
  return DistributionLogging {
    dlEnabled        = en
  , dlIncludeCookies = ic
  , dlBucket         = b
  , dlPrefix         = lp
  }


-------------------------------------------------------------------------------
parseViewerCertificate
    :: (Functor m, MonadThrow m)
    => X.Cursor
    -> EitherT Text m ViewerCertificate
parseViewerCertificate cursor = do
  mcid <- ftMaybe $ cursor
         $/ le "IAMCertificateId"
         &/ X.content
  strat <- case mcid of
    Just cid -> do
      sm <- getContentOf cursor "SSLSupportMethod"
      return $ UseIAM cid sm
    Nothing -> return CloudFrontDefaultCertificate
  mpv <- ftMaybe $ cursor
         $/ le "MinimumProtocolVersion"
         &/ X.content
  return ViewerCertificate {
    vcCertificateStrategy    = strat
  , vcMinimumProtocolVersion = mpv
  }
