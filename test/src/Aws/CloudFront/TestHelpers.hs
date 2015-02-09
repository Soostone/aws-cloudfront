{-# LANGUAGE OverloadedStrings #-}
module Aws.CloudFront.TestHelpers where

-------------------------------------------------------------------------------
import           Data.Default
import           Filesystem.Path
import           Prelude         hiding (FilePath)
import qualified Text.XML        as X
import qualified Text.XML.Cursor as X
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


parseFixture :: FilePath -> (X.Cursor -> a) -> IO a
parseFixture fp f = do
  doc <- X.readFile def $ fixturePath fp
  return $ f $ X.fromDocument doc


-------------------------------------------------------------------------------
fixturePath :: FilePath -> FilePath
fixturePath fp = "test" </> "fixtures" </> fp
