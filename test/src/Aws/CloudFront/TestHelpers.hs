{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Aws.CloudFront.TestHelpers where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Default
import           Data.List.NonEmpty    (NonEmpty (..))
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

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = (:|) <$> arbitrary <*> arbitrary


instance Arbitrary CreateInvalidationRequest where
  arbitrary = CreateInvalidationRequest
              <$> arbitrary
              <*> arbitrary
              <*> arbitrary
