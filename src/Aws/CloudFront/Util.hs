module Aws.CloudFront.Util where


-------------------------------------------------------------------------------
import           Control.Applicative
import           Data.String
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Text.Parser.Char    as PC
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | AwsType Helpers
-------------------------------------------------------------------------------

toTextText :: (IsString a) => Text -> a
toTextText = fromString . T.unpack

parseTextText :: (PC.CharParsing f) => f Text
parseTextText = T.pack <$> parseString

parseString :: (PC.CharParsing f) => f String
parseString = many PC.anyChar
