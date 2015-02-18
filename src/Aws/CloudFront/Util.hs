{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Aws.CloudFront.Util where


-------------------------------------------------------------------------------
import           Aws.Core
import           Aws.General
import           Control.Applicative
import           Control.Error
import           Control.Monad.Catch
import           Data.Monoid
import qualified Data.Set                as S
import           Data.String
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Time
import           System.Locale
import qualified Text.Parser.Char        as PC
import qualified Text.Parser.Combinators as PC
import           Text.XML.Cursor         (($/), ($|), (&/))
import qualified Text.XML.Cursor         as X
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | AwsType Helpers
-------------------------------------------------------------------------------

toTextText :: (IsString a) => Text -> a
toTextText = fromString . T.unpack


-------------------------------------------------------------------------------
parseTextText :: (PC.CharParsing f) => f Text
parseTextText = T.pack <$> parseString


-------------------------------------------------------------------------------
parseString :: (PC.CharParsing f) => f String
parseString = many PC.anyChar


-------------------------------------------------------------------------------
fromText' :: (AwsType a) => Text -> Either Text a
fromText' = fmapL T.pack . fromText


-------------------------------------------------------------------------------
--TODO: quickcheck, move to util
parseInt :: (Applicative m, PC.CharParsing m) => m Int
parseInt = (read <$> some PC.digit) <* PC.eof


-------------------------------------------------------------------------------
-- | Text Helpers
-------------------------------------------------------------------------------

-- | All characters in string are members of the given whitelist
whitelistText :: String -> Text -> Bool
whitelistText bl = T.all chk
  where
    bl' = S.fromList bl
    chk = flip S.member bl'


-------------------------------------------------------------------------------
-- | XML Helpers
-------------------------------------------------------------------------------

getContentOf
    :: (Functor m, MonadThrow m, AwsType a)
    => X.Cursor
    -> Text
    -> EitherT Text m a
getContentOf c n = EitherT $ do
  e <- force ("Missing element " <> T.unpack n) $ c $/ le n
  return . fromText' . fromMaybe mempty . listToMaybe $ e $/ X.content


-------------------------------------------------------------------------------
parseFirst :: (MonadThrow m) => X.Cursor -> Text -> (X.Cursor -> m b) -> m b
parseFirst cursor n parser = do
  res <- force ("Missing Element " <> T.unpack n) $ cursor $/ le n
  res $| parser



-------------------------------------------------------------------------------
le :: Text -> X.Axis
le = X.laxElement


-------------------------------------------------------------------------------
newtype AWSUTCTime = AWSUTCTime {
      unAWSUTCTime :: UTCTime
    } deriving (ParseTime, Show, Eq)


-------------------------------------------------------------------------------
instance AwsType AWSUTCTime where
  toText = fromString . formatTime defaultTimeLocale awsTimeFmt . unAWSUTCTime
  parse = do
    s <- parseString
    maybe (fail $ "could not parse UTCTime string " <> s) return $ parseTime defaultTimeLocale awsTimeFmt s


-------------------------------------------------------------------------------
awsTimeFmt :: String
awsTimeFmt = "%Y-%m-%dT%H:%M:%S%QZ"


-------------------------------------------------------------------------------
newtype AWSBool = AWSBool {
      unAWSBool :: Bool
    } deriving (Show, Eq)


instance AwsType AWSBool where
  toText (AWSBool True) = "true"
  toText (AWSBool False) = "false"
  parse = (AWSBool True <$ PC.text "true") <|>
          (AWSBool False <$ PC.text "false")


-------------------------------------------------------------------------------
--TODO: off to Util with ye
ftMaybe :: (Monad m, AwsType a) => [Text] -> EitherT Text m (Maybe a)
ftMaybe [] = return Nothing
ftMaybe (x:_) = hoistEither (Just <$> fromText' x)


-------------------------------------------------------------------------------
--TODO: make sure this is used everywhere it needs to be, extract to Util
ftList
    :: (MonadThrow m, AwsType a)
    => X.Cursor
    -> Text
    -> EitherT Text m [a]
ftList cursor n =
  hoistEither . mapM fromText' $ cursor
    $/ le "Items"
    &/ le n
    &/ X.content
