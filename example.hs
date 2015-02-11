{-# LANGUAGE OverloadedStrings #-}
module Example where

-------------------------------------------------------------------------------
import           Aws
import           Control.Applicative
-- import           Data.List.NonEmpty
-------------------------------------------------------------------------------
import           Aws.CloudFront
-------------------------------------------------------------------------------


example :: IO ()
example = do
    cfg <- setDebug <$> Aws.baseConfiguration
    -- let cfCfg = CloudFrontConfiguration :: ServiceConfiguration CreateInvalidationRequest NormalQuery
    let cfCfg' = CloudFrontConfiguration :: ServiceConfiguration GetDistributionListRequest NormalQuery
    -- print =<< simpleAws cfg cfCfg (CreateInvalidationRequest paths cr did)
    print =<< simpleAws cfg cfCfg' (GetDistributionListRequest Nothing)
  where
    -- paths = ObjectPath "/bogus.xml" :| []
    -- cr = CreateInvalidationRequestReference "bogusref"
    -- did = DistributionId "bogus-did"
    setDebug cfg = cfg { logger = defaultLog Debug}

