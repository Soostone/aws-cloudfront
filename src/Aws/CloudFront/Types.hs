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
    -- * XML Parsers
    , parseInvalidation
    ) where

-------------------------------------------------------------------------------
import           Aws.Core
import           Aws.General
import           Control.Applicative
import           Control.Error
import           Control.Monad.Catch
import           Data.List.NonEmpty  (NonEmpty (..))
import           Data.Monoid
import           Data.Text           (Text)
import           Data.Time
import           Data.Traversable
import           Data.Typeable
import qualified Text.Parser.Char    as PC
import           Text.XML.Cursor     (($/), (&/))
import qualified Text.XML.Cursor     as X
-------------------------------------------------------------------------------
import           Aws.CloudFront.Core
import           Aws.CloudFront.Util
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
