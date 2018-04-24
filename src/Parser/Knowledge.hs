module Parser.Knowledge
    (
      parseKnowledge
    ) where

import qualified Data.Text.Lazy      as LT

import           Exceptions
import           Parser.Util
import           Types

-- | Parse description
parseKDesc' :: LT.Text -> Either KBError (LT.Text, LT.Text, LT.Text)
parseKDesc' txt = case LT.lines txt of
                      (_:problem:_:locale:_:solution) ->
                          return (problem, locale, LT.unlines solution)
                      _                               -> Left InvalidFormat
-- | Parse description
parseKDesc :: LT.Text -> Either KBError KDescription
parseKDesc txt = do
    (problem, locale, solution) <- parseKDesc' txt
    parsedLocale <- parseLocale locale
    return $ KDescription parsedLocale problem solution

-- | Parse knowledge's meta data
parseKnowledgeMeta :: LT.Text -> Either KBError (LT.Text, LT.Text, LT.Text)
parseKnowledgeMeta txt = case LT.lines txt of
                             (_:eCode:_:eCategory:_:eText:_) ->
                                  return (eCode, eCategory, eText)
                             _               -> Left InvalidFormat

-- | Parse knowledge directory
parseKnowledge :: [LT.Text] -> LT.Text -> Either KBError Knowledge
parseKnowledge ds metadata = do
    descriptions <- mapM parseKDesc ds
    (eCode, eCategory, eText) <- parseKnowledgeMeta metadata
    category <- parseCategory eCategory
    return $ Knowledge eCode category eText descriptions
