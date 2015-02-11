module Aws.CloudFront.Util where


-------------------------------------------------------------------------------
import           Control.Applicative
import qualified Data.Set            as S
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


-------------------------------------------------------------------------------
-- | Text Helpers
-------------------------------------------------------------------------------

-- | All characters in string are members of the given whitelist
whitelistText :: String -> Text -> Bool
whitelistText bl = T.all chk
  where
    bl' = S.fromList bl
    chk = flip S.member bl'
