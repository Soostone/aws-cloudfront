{-# LANGUAGE OverloadedStrings #-}
module Example where

-------------------------------------------------------------------------------
import Aws
import Data.List.NonEmpty
-------------------------------------------------------------------------------
import Aws.CloudFront
-------------------------------------------------------------------------------


example :: IO ()
example = do
    cfg <- Aws.baseConfiguration
    let cfCfg = CloudFrontConfiguration
    print =<< simpleAws cfg cfCfg (CreateInvalidationRequest paths cr did)
  where
    paths = ObjectPath "/bogus.xml" :| []
    cr = CreateInvalidationRequestReference "bogusref"
    did = DistributionId "bogus-did"

