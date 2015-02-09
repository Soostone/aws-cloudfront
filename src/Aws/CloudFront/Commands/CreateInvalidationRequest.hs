{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

--TODO: probably remove
{-# LANGUAGE ScopedTypeVariables        #-}
-- |
-- Module: Aws.CloudFront.Commands.CreateInvalidationRequest
-- Copyright: Copyright © 2015 SooStone, Inc.
-- License: MIT
-- Maintainer: Michael Xavier <michael.xavier@soostone.com>
-- Stability: experimental
--
-- /API Version: 2014-11-06/
--
-- Create an invalidation batch request. This removes an object from
-- edge-server caches before the scheduled expiry.
-- <http://docs.aws.amazon.com/AmazonCloudFront/latest/APIReference/CreateInvalidation.html>
module Aws.CloudFront.Commands.CreateInvalidationRequest
    ( CreateInvalidationRequest(..)
    , CreateInvalidationRequestReference(..)
    , ObjectPath(..)
    , CreateInvalidationResponse(..)
    , Invalidation(..)
    , InvalidationStatus(..)
    , InvalidationId(..)
    -- * Exported for testing
    , parseInvalidation
    ) where


-------------------------------------------------------------------------------
import           Aws.Core
import           Aws.General
import           Control.Applicative
import           Control.Error
import           Control.Monad.Trans.Resource (throwM)
import           Data.List.NonEmpty           (NonEmpty (..))
import           Data.Monoid
import           Data.String
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Time
import           Data.Traversable
import           Data.Typeable
import           System.Locale
import qualified Text.Parser.Char             as PC
import qualified Text.XML                     as X
import           Text.XML.Cursor              (($/), ($//), (&/), (&//))
import qualified Text.XML.Cursor              as X
-------------------------------------------------------------------------------
import           Aws.CloudFront.Core
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
--TODO: smart constructor nonempty, leading slash, urlencode
newtype ObjectPath = ObjectPath {
      objectPathText :: Text
    } deriving (Show, Eq, Ord, Monoid, Typeable)

instance AwsType ObjectPath where
  toText = toTextText . objectPathText
  parse = ObjectPath <$> parseTextText


-------------------------------------------------------------------------------
data CreateInvalidationRequest = CreateInvalidationRequest {
      cirPaths           :: NonEmpty ObjectPath --TODO: use a stronger path type?
    , cirCallerReference :: CreateInvalidationRequestReference
    , cirDistributionId  :: DistributionId
    } deriving (Show, Eq, Ord, Typeable)

--TODO: returns an InvalidationResponse which is identical to the show response

instance SignQuery CreateInvalidationRequest

instance Transaction CreateInvalidationRequest CreateInvalidationResponse

-------------------------------------------------------------------------------
newtype CreateInvalidationResponse = CreateInvalidationResponse {
      ciresInvalidation :: Invalidation
    } deriving (Show, Eq, Ord, Typeable)

instance AsMemoryResponse CreateInvalidationResponse where
  type MemoryResponse CreateInvalidationResponse = CreateInvalidationResponse
  loadToMemory = return

instance ResponseConsumer r CreateInvalidationResponse where
  type ResponseMetadata CreateInvalidationResponse = CloudFrontMetadata
  responseConsumer _ = cloudFrontXmlResponseConsumer parse
    where
      parse cursor = do
        res <- runEitherT $ parseInvalidation cursor
        case res of
          Left e -> throwM $ CloudFrontResponseDecodeError $ formatError e
          Right r -> return $ CreateInvalidationResponse r
      formatError e = "Failed to parse cloudfront response: " <> (T.pack . show) e
      --TODO: probably extract

parseInvalidation cursor = do
  cloudFrontCheckResponseType () "Invalidation" cursor
  i <- getContentOf cursor "Id"
  stat <- getContentOf cursor "Status"
  ct <- awsUTCTime <$> getContentOf cursor "CreateTime"
  batch :: X.Cursor <- force "Missing InvalidationBatch" $ cursor
                       $/ X.laxElement "InvalidationBatch"
  cref <- getContentOf batch "CallerReference"
  paths :: [Text] <- right $ batch
                            $/ X.laxElement "Paths"
                            &/ X.laxElement "Items" --FIXME
                            &/ X.laxElement "Path"
                            &/ X.content
  pathsNE <- case paths of
    (x:xs) -> hoistEither (traverse fromText' $ x :| xs)
    _ -> throwT "Empty Paths tag"
  return $ Invalidation { invStatus = stat
                        , invPaths = pathsNE
                        , invCallerReference = cref
                        , invInvalidationId = i
                        , invCreateTime = ct
                        }

getContentOf c n = EitherT . fmap fromText' . force ("Missing element " <> T.unpack n) $
                   c $/ X.laxElement n &/ X.content


-------------------------------------------------------------------------------
--todo: extract and share in a common types module
data Invalidation = Invalidation {
      invStatus          :: InvalidationStatus
    , invPaths           :: NonEmpty ObjectPath
    , invCallerReference :: CreateInvalidationRequestReference
    , invInvalidationId  :: InvalidationId
    , invCreateTime      :: UTCTime
    } deriving (Show, Eq, Ord, Typeable)


-------------------------------------------------------------------------------
data InvalidationStatus = InvalidationInProgress
                        | InvalidationCompleted
                        deriving (Show, Eq, Ord, Typeable)


instance AwsType InvalidationStatus where
  toText InvalidationInProgress = "InProgress"
  toText InvalidationCompleted = "Completed"
  parse = parseInProgress <|>
          parseCompleted
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
instance AwsType AWSUTCTime where
  toText = fromString . formatTime defaultTimeLocale awsTimeFmt . awsUTCTime
  parse = do
    s <- parseString
    maybe (fail "could not parse UTCTime") return $ parseTime defaultTimeLocale awsTimeFmt s


-------------------------------------------------------------------------------
awsTimeFmt :: String
awsTimeFmt = "%Y-%m-%dT%H:%M:%SZ"


-------------------------------------------------------------------------------
newtype AWSUTCTime = AWSUTCTime { awsUTCTime :: UTCTime } deriving (ParseTime)


toTextText = fromString . T.unpack
parseTextText = T.pack <$> parseString
parseString = many PC.anyChar

--TODO: error cases?


-------------------------------------------------------------------------------
fromText' :: (AwsType a) => Text -> Either Text a
fromText' = fmapL T.pack . fromText
