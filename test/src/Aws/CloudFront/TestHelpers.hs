{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Aws.CloudFront.TestHelpers where

-------------------------------------------------------------------------------
import           Aws.General
import           Control.Applicative
import           Data.Default
import           Data.Derive.Arbitrary
import           Data.DeriveTH
import           Data.List.NonEmpty    (NonEmpty (..))
import           Data.Maybe
import           Data.String
import qualified Data.Text             as T
import           Data.Time.Calendar
import           Data.Time.Clock
import           Filesystem.Path
import           Prelude               hiding (FilePath)
import           Test.Tasty.QuickCheck
import qualified Text.XML              as X
import qualified Text.XML.Cursor       as X
-------------------------------------------------------------------------------
import           Aws.CloudFront
import           Aws.CloudFront.Util
-------------------------------------------------------------------------------


(<||>) :: (Applicative f) => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)


-------------------------------------------------------------------------------
parseFixture :: FilePath -> (X.Cursor -> a) -> IO a
parseFixture fp f = do
  doc <- X.readFile def $ fixturePath fp
  return $ f $ X.fromDocument doc


-------------------------------------------------------------------------------
fixturePath :: FilePath -> FilePath
fixturePath fp = "test" </> "fixtures" </> fp


instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = (:|) <$> arbitrary <*> arbitrary


instance Arbitrary HTTPPort where
  arbitrary = fromJust . mkHTTPPort <$> go
    where
      go = arbitrary `suchThat` (isJust . mkHTTPPort)


instance Arbitrary CountryCode where
  arbitrary = do
    a <- choose ('A','Z')
    b <- choose ('A','Z')
    return $ fromJust $ mkCountryCode $ T.pack [a,b]


instance Arbitrary MinTTL where
  arbitrary = fromJust . mkMinTTL <$> go
    where
      go = arbitrary `suchThat` (isJust . mkMinTTL)


--TODO: rewrite others in this style?
instance Arbitrary LoggingPrefix where
  arbitrary = fromJust . mkLoggingPrefix <$> go
    where
      go = arbitrary `suchThat` (isJust . mkLoggingPrefix)


instance Arbitrary PathPattern  where
  arbitrary = fromJust . mkPathPattern <$> go
    where
      go = arbitrary `suchThat` (isJust . mkPathPattern)


instance Arbitrary BucketName  where
  arbitrary = fromJust . mkBucketName <$> go
    where
      go = arbitrary `suchThat` (isJust . mkBucketName)


$(derive makeArbitrary ''Marker)
$(derive makeArbitrary ''S3OriginConfig)
$(derive makeArbitrary ''OriginAccessIdentity)
$(derive makeArbitrary ''CreateInvalidationRequest)
$(derive makeArbitrary ''OriginProtocolPolicy)
$(derive makeArbitrary ''ObjectPath)
$(derive makeArbitrary ''CreateInvalidationRequestReference)
$(derive makeArbitrary ''DistributionId)
$(derive makeArbitrary ''InvalidationId)
$(derive makeArbitrary ''InvalidationStatus)
$(derive makeArbitrary ''CloudFrontAction)
$(derive makeArbitrary ''AWSUTCTime)
$(derive makeArbitrary ''AWSBool)
$(derive makeArbitrary ''ErrorCode)
$(derive makeArbitrary ''ResponseCode)
$(derive makeArbitrary ''MinimumProtocolVersion)
$(derive makeArbitrary ''SSLSupportMethod)
$(derive makeArbitrary ''IAMCertificateId)
$(derive makeArbitrary ''PriceClass)
$(derive makeArbitrary ''OriginId)
$(derive makeArbitrary ''ViewerProtocolPolicy)
$(derive makeArbitrary ''CookieName)
$(derive makeArbitrary ''HeaderName)
$(derive makeArbitrary ''DomainName)
$(derive makeArbitrary ''DistributionStatus)
$(derive makeArbitrary ''AccountNumber)


-------------------------------------------------------------------------------
prop_AWSType_cycle :: (AwsType a, Arbitrary a, Show a, Eq a) => a -> Property
prop_AWSType_cycle a = fromText (fromString (toText a)) === Right a


-------------------------------------------------------------------------------
mkUTCTime :: Integer -> Int -> Int -> DiffTime -> DiffTime -> DiffTime -> UTCTime
mkUTCTime yr mt dy hr mn sc = UTCTime d t
  where
    d = fromGregorian yr mt dy
    t = hr * 60 * 60 + mn * 60 + sc
