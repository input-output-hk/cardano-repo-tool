module RepoTool.Text
  ( concatParts
  , splitIntoParts
  ) where

import qualified Data.Char as Char
import           Data.Text (Text)
import qualified Data.Text as Text

import           RepoTool.Types (TextPart (..))


splitIntoParts :: Text -> [TextPart]
splitIntoParts txt =
  case Text.span Char.isSpace txt of
    (space, rest) ->
      if Text.null space
        then takeReadable rest
        else TextWhitespace space : takeWhitespace rest

concatParts :: [TextPart] -> Text
concatParts tps =
  mconcat $ map convert tps
 where
  convert tp =
    case tp of
      TextWhitespace txt -> txt
      TextReadable txt -> txt
      TextGitHash txt -> txt

-- -----------------------------------------------------------------------------

takeWhitespace :: Text -> [TextPart]
takeWhitespace txt =
  case Text.span Char.isSpace txt of
    (space, rest) ->
      if Text.null rest
        then [TextWhitespace space]
        else TextWhitespace space : takeReadable rest

takeReadable :: Text -> [TextPart]
takeReadable txt =
  case Text.span (not . Char.isSpace) txt of
    (readable, rest) ->
      if Text.null rest
        then [mkReadable readable]
        else mkReadable readable : takeWhitespace rest

mkReadable :: Text -> TextPart
mkReadable txt
  | isGitHash txt = TextGitHash txt
  | otherwise = TextReadable txt

isGitHash :: Text -> Bool
isGitHash txt =
  Text.length txt == 40 && Text.all Char.isHexDigit txt
