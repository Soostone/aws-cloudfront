{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Aws.CloudFront.Tests.Commands.GetDistributionList
    ( tests
    ) where


-------------------------------------------------------------------------------
import           Control.Error
import           Data.List.NonEmpty                          (NonEmpty (..))
import           Data.Maybe
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
-------------------------------------------------------------------------------
import           Aws.CloudFront.Commands.GetDistributionList
import           Aws.CloudFront.Core
import           Aws.CloudFront.TestHelpers
import           Aws.General
-------------------------------------------------------------------------------


tests :: TestTree
tests = testGroup "Aws.CloudFront.Commands.GetDistributionList"
  [ parseDistributionListResponseTests
  ]


-------------------------------------------------------------------------------
parseDistributionListResponseTests :: TestTree
parseDistributionListResponseTests = testGroup "parseDistributionListResponse"
  [ testCase "it parses the example xml" $ do
      res <- runEitherT =<< parseFixture "ListDistributions.xml" parseDistributionListResponse
      res @?= Right expectedDistributionListResponse
  , testProperty "AwsType DistributionStatus" $ \(x :: DistributionStatus) ->
     prop_AWSType_cycle x
  , testProperty "AwsType DomainName" $ \(x :: DomainName) ->
     prop_AWSType_cycle x
  , testProperty "AwsType PathPattern" $ \(x :: PathPattern) ->
     prop_AWSType_cycle x
  , testProperty "AwsType HeaderName" $ \(x :: HeaderName) ->
     prop_AWSType_cycle x
  , testProperty "AwsType CookieName" $ \(x :: CookieName) ->
     prop_AWSType_cycle x
  , testProperty "AwsType AccountNumber" $ \(x :: AccountNumber) ->
     prop_AWSType_cycle x
  , testProperty "AwsType ViewerProtocolPolicy" $ \(x :: ViewerProtocolPolicy) ->
     prop_AWSType_cycle x
  , testProperty "AwsType MinTTL" $ \(x :: MinTTL) ->
     prop_AWSType_cycle x
  , testProperty "AwsType LoggingPrefix" $ \(x :: LoggingPrefix) ->
     prop_AWSType_cycle x
  , testProperty "AwsType CountryCode" $ \(x :: CountryCode) ->
     prop_AWSType_cycle x
  , testProperty "AwsType PriceClass" $ \(x :: PriceClass) ->
     prop_AWSType_cycle x
  , testProperty "AwsType IAMCertificateId" $ \(x :: IAMCertificateId) ->
     prop_AWSType_cycle x
  , testProperty "AwsType SSLSupportMethod" $ \(x :: SSLSupportMethod) ->
     prop_AWSType_cycle x
  , testProperty "AwsType MinimumProtocolVersion" $ \(x :: MinimumProtocolVersion) ->
     prop_AWSType_cycle x
  , testProperty "AwsType OriginId" $ \(x :: OriginId) ->
     prop_AWSType_cycle x
  , testProperty "AwsType ResponseCode" $ \(x :: ResponseCode) ->
     prop_AWSType_cycle x
  , testProperty "AwsType ErrorCode" $ \(x :: ErrorCode) ->
     prop_AWSType_cycle x
  , testProperty "AwsType S3OriginConfig" $ \(x :: S3OriginConfig) ->
     prop_AWSType_cycle x
  , testProperty "AwsType OriginAccessIdentity" $ \(x :: OriginAccessIdentity) ->
     prop_AWSType_cycle x
  , testProperty "AwsType OriginProtocolPolicy" $ \(x :: OriginProtocolPolicy) ->
     prop_AWSType_cycle x
  , testProperty "AwsType HTTPPort" $ \(x :: HTTPPort) ->
     prop_AWSType_cycle x
  ]


-------------------------------------------------------------------------------
expectedDistributionListResponse :: GetDistributionListResponse
expectedDistributionListResponse = GetDistributionListResponse {
      gdlresCurMarker = Just $ Marker "RMPARXS293KSTG7"
    , gdlresNextMarker = Just $ Marker "EMLARXS9EXAMPLE"
    , gdlresIsTruncated = True
    , gdlresSummaries = [expectedSummary]
    }


-------------------------------------------------------------------------------
expectedSummary :: DistributionSummary
expectedSummary = DistributionSummary {
      dsId = DistributionId "EDFDVBD6EXAMPLE"
    , dsStatus = DistributionDeployed
    , dsLastModifiedTime = mkUTCTime 2012 5 19 19 37 58
    , dsDomainName = DomainName "d111111abcdef8.cloudfront.net"
    , dsAliases = [CName "www.example.com"]
    , dsOrigins = [s3Origin, customOrigin]
    , dsDefaultCacheBehavior  = defaultCacheBehavior
    , dsCacheBehaviors = [cacheBehavior]
    , dsCustomErrorResponses = [customErrorResponse]
    , dsRestrictions = Just geoRestriction
    , dsComment = Just "example comment"
    , dsLogging = distributionLogging
    , dsViewerCertificate = viewerCertificate
    , dsPriceClass = PriceClassAll
    , dsEnabled = True
    }


-------------------------------------------------------------------------------
s3Origin :: Origin
s3Origin = Origin {
      originId = OriginId "example-Amazon S3-origin"
    , originDomainName = DomainName "myawsbucket.s3.amazonaws.com"
    , originPath = Just $ ObjectPath "/production"
    , originConfig = Left $ S3OriginConfig $ OriginAccessIdentity "E74FTE3AEXAMPLE"
    }


-------------------------------------------------------------------------------
customOrigin :: Origin
customOrigin = Origin {
      originId = OriginId "example-custom-origin"
    , originDomainName = DomainName "example.com"
    , originPath = Nothing
    , originConfig = Right coc
    }
  where
    coc = fromJust $ do
      httpPort <- mkHTTPPort 80
      httpsPort <- mkHTTPPort 443
      return CustomOriginConfig {
            cocHTTPPort             = Just httpPort
          , cocHTTPSPort            = Just httpsPort
          , cocOriginProtocolPolicy = OPPMatchViewer
          }


-------------------------------------------------------------------------------
defaultCacheBehavior :: UnqualifiedCacheBehavior
defaultCacheBehavior = fromJust $ do
    mtl <- mkMinTTL 0
    return UnqualifiedCacheBehavior {
        cbTargetOriginId       = OriginId "example-Amazon S3-origin"
      , cbForwardedValues      = fvs
      , cbTrustedSigners       = TrustedSignersEnabled ans
      , cbViewerProtocolPolicy = VPPRedirectToHTTPS
      , cbMinTTL               = mtl
      , cbAllowedMethods       = Just $ AllowedMethods AMGH CMGH
      , cbSmoothStreaming      = False
      }
  where
    fvs = ForwardedValues {
            fvQueryString = True
          , fvCookies = CookieWhitelist $ CookieName "example-cookie" :| []
          , fvHeaders = [HeaderName "Origin"]
          }
    ans = SelfAccountNumber :| [ OtherAccountNumber (AccountId "111122223333")
                               , OtherAccountNumber (AccountId "444455556666")
                               ]


-------------------------------------------------------------------------------
cacheBehavior :: CacheBehavior
cacheBehavior = fromJust $ do
    pp <- mkPathPattern "*.jpg"
    return CacheBehavior {
      cbPathPattern = pp
    , cbBehavior = b
    }
  where
    fvs = ForwardedValues {
            fvQueryString = False
          , fvCookies = ForwardAllCookies
          , fvHeaders = [HeaderName "Origin"]
          }
    ans =  SelfAccountNumber :| [OtherAccountNumber (AccountId "111122223333")]
    b = fromJust $ do
      mtl <- mkMinTTL 86400
      return UnqualifiedCacheBehavior {
               cbTargetOriginId       = OriginId "example-custom-origin"
             , cbForwardedValues      = fvs
             , cbTrustedSigners       = TrustedSignersEnabled ans
             , cbViewerProtocolPolicy = VPPAllowAll
             , cbMinTTL               = mtl
             , cbAllowedMethods       = Just $ AllowedMethods AMGH CMGH
             , cbSmoothStreaming      = False
             }


-------------------------------------------------------------------------------
customErrorResponse :: CustomErrorResponse
customErrorResponse = fromJust $ do
  mtl <- mkMinTTL 30
  return CustomErrorResponse {
    cerErrorCode = EC404
  , cerResponsePagePath = Just $ ObjectPath "/error-pages/404.html"
  , cerResponseCode = RC200
  , cerMinTTL = Just mtl
  }


-------------------------------------------------------------------------------
distributionLogging :: DistributionLogging
distributionLogging = fromJust $ do
  bn <- mkBucketName "myawslogbucket.s3.amazonaws.com"
  lp <- mkLoggingPrefix "example.com."
  return DistributionLogging {
    dlEnabled        = True
  , dlIncludeCookies = True
  , dlBucket         = bn
  , dlPrefix         = lp
  }


-------------------------------------------------------------------------------
viewerCertificate :: ViewerCertificate
viewerCertificate = ViewerCertificate {
      vcCertificateStrategy = UseIAM (IAMCertificateId "AS1A2M3P4L5E67SIIXR3J") VIP
    , vcMinimumProtocolVersion = Just MinTLSv1
    }


-------------------------------------------------------------------------------
geoRestriction :: GeoRestriction
geoRestriction = fromJust $ do
  aq <- mkCountryCode "AQ"
  cv <- mkCountryCode "CV"
  return $ GeoWhitelist $ aq :| [cv]
