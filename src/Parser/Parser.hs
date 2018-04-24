module Parser.Parser
    (
      Parser(..)
    , parseKnowledge
    , parseQuestion
    ) where

import qualified Data.Text.Lazy   as LT

import           Exceptions

import           Parser.Knowledge
import           Parser.Question

newtype Parser a = Parser {runParser :: [LT.Text] -> LT.Text -> Either KBError a }
