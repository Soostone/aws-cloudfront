{-# LANGUAGE OverloadedStrings #-}
module Aws.CloudFront.Tests.Commands.GetDistributionList
    ( tests
    ) where


-------------------------------------------------------------------------------
import           Control.Error
import           Data.Maybe
-- import           Data.List.NonEmpty                          (NonEmpty (..))
import           Test.Tasty
import           Test.Tasty.HUnit
-- import           Test.Tasty.QuickCheck
-------------------------------------------------------------------------------
import           Aws.CloudFront.Commands.GetDistributionList
import           Aws.CloudFront.Core
import           Aws.CloudFront.TestHelpers
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
  ]


-------------------------------------------------------------------------------
expectedDistributionListResponse :: GetDistributionListResponse
expectedDistributionListResponse = GetDistributionListResponse {
      gdlresCurMarker = Marker "RMPARXS293KSTG7"
    , gdlresNextMarker = Marker "EMLARXS9EXAMPLE"
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
    , originConfig = Left $ S3OriginConfig "E74FTE3AEXAMPLE"
    }


-------------------------------------------------------------------------------
customOrigin :: Origin
customOrigin = Origin {
      originId = OriginId "example-custom-origin"
    , originDomainName = DomainName "exmaple.com"
    , originPath = Nothing
    , originConfig = Right coc
    }
  where
    coc = fromJust $ do
      httpPort <- mkHTTPPort 80
      httpsPort <- mkHTTPPort 443
      CustomOriginConfig {
            cocHTTPPort             = Just httpPort
          , cocHTTPSPort            = Just httpsPort
          , cocOriginProtocolPolicy = OPPMatchViewer
          }


-------------------------------------------------------------------------------
defaultCacheBehavior :: UnqualifiedCacheBehavior
defaultCacheBehavior = undefined


-------------------------------------------------------------------------------
cacheBehavior :: CacheBehavior
cacheBehavior = undefined


-------------------------------------------------------------------------------
customErrorResponse :: CustomErrorResponse
customErrorResponse = undefined


-------------------------------------------------------------------------------
distributionLogging :: DistributionLogging
distributionLogging = fromJust $ do
  return DistributionLog {
    
  }


-------------------------------------------------------------------------------
viewerCertificate :: ViewerCertificate
viewerCertificate = ViewerCertificate {
      vcCertificateStrategy = UseIAM (IAMCertificateId "AS1A2M3P4L5E67SIIXR3J" VIP)
    , vcMinimumProtocolVersion = Just MinTLSv1
    }


-------------------------------------------------------------------------------
geoRestriction :: GeoRestriction
geoRestriction = fromJust $ do
  aq <- mkCountryCode "AQ"
  cv <- mkCountryCode "CV"
  return $ GeoWhitelist $ aw :| [cv]
