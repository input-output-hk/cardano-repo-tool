{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.RepoTool.Text
  ( tests
  ) where


import qualified Data.Char as Char
import           Data.Text (Text)
import qualified Data.Text as Text

import           Hedgehog (Gen, Property, (===), discover)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           RepoTool


prop_roundtrip_random_text :: Property
prop_roundtrip_random_text =
  H.withTests 1000 . H.property $ do
    txt <- H.forAll genRandomText
    let nonEmpty = not $ Text.null txt
        startChar = fst <$> Text.uncons txt
        startSpace = maybe False Char.isSpace startChar

        parts = splitIntoParts txt
        concated = concatParts parts

    concated === txt

    H.cover 90 "  non-empty" nonEmpty
    H.cover 30 "  starts with space" startSpace
    H.cover 30 "  starts with non-space" (not startSpace)
    H.cover 70 "  non-trvial" (length parts > 5)

-- -----------------------------------------------------------------------------

genRandomText :: Gen Text
genRandomText =
  Text.concat <$> Gen.list (Range.linear 0 100) generator
 where
  generator =
    Gen.choice
      [ Text.append <$> genTextChunk <*> genWhitespace
      , Text.append <$> genWhitespace <*> genTextChunk
      ]

genTextChunk :: Gen Text
genTextChunk =
  Text.pack <$> Gen.string (Range.linear 0 100) Gen.unicode

genWhitespace :: Gen Text
genWhitespace =
  Text.pack <$> Gen.string (Range.linear 0 10) (Gen.element " \t\r\n\v")

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkParallel $$discover
