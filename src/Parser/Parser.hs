module Parser.Parser
    (
      MdParser
    , runParser
    , mdParser
    , parseKnowledge
    , parseQuestion
    ) where

import qualified Data.Text.Lazy   as LT

import           Exceptions

import           Parser.Knowledge (parseKnowledge)
import           Parser.Question  (parseQuestion)

newtype MdParser a = MdParser {runParser :: [LT.Text] -> LT.Text -> Either KBError a }

mdParser :: ([LT.Text] -> LT.Text -> Either KBError a) -> MdParser a
mdParser = MdParser
