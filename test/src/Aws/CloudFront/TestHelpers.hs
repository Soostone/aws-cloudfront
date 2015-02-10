{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Aws.CloudFront.TestHelpers where

-------------------------------------------------------------------------------
import           Aws.General
import           Control.Applicative
import           Data.Default
import           Data.List.NonEmpty    (NonEmpty (..))
import           Data.String
import           Filesystem.Path
import           Prelude               hiding (FilePath)
import           Test.Tasty.QuickCheck
import qualified Text.XML              as X
import qualified Text.XML.Cursor       as X
-------------------------------------------------------------------------------
import           Aws.CloudFront
-------------------------------------------------------------------------------


parseFixture :: FilePath -> (X.Cursor -> a) -> IO a
parseFixture fp f = do
  doc <- X.readFile def $ fixturePath fp
  return $ f $ X.fromDocument doc


-------------------------------------------------------------------------------
fixturePath :: FilePath -> FilePath
fixturePath fp = "test" </> "fixtures" </> fp


instance Arbitrary ObjectPath where
  arbitrary = ObjectPath <$> arbitrary


instance Arbitrary CreateInvalidationRequestReference where
  arbitrary = CreateInvalidationRequestReference <$> arbitrary


instance Arbitrary DistributionId where
  arbitrary = DistributionId <$> arbitrary


instance Arbitrary InvalidationId where
  arbitrary = InvalidationId <$> arbitrary


instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = (:|) <$> arbitrary <*> arbitrary


instance Arbitrary CreateInvalidationRequest where
  arbitrary = CreateInvalidationRequest
              <$> arbitrary
              <*> arbitrary
              <*> arbitrary


instance Arbitrary InvalidationStatus where
  arbitrary = oneof [ pure InvalidationInProgress
                    , pure InvalidationCompleted
                    ]


instance Arbitrary CloudFrontAction where
  arbitrary = oneof [ pure CreateInvalidation
                    ]


-------------------------------------------------------------------------------
prop_AWSType_cycle :: (AwsType a, Arbitrary a, Show a, Eq a) => a -> Property
prop_AWSType_cycle a = fromText (fromString (toText a)) === Right a
