module Parser.Parser
    ( DocParser
    , runParser
    , docParser
    , parseKnowledge
    , parseFAQ
    ) where

import qualified Data.Text.Lazy   as LT

import           Exceptions

import           Parser.Knowledge (parseKnowledge)
import           Parser.FAQ       (parseFAQ)

newtype DocParser a = DocParser { runParser :: [LT.Text] -> LT.Text -> Either KBError a }

docParser :: ([LT.Text] -> LT.Text -> Either KBError a) -> DocParser a
docParser = DocParser
