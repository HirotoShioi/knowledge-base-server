{-# LANGUAGE RecordWildCards #-}

module Parser.FAQ
     ( parseFAQ
     ) where

import qualified Data.Text.Lazy as LT

import           Exceptions
import           Parser.Util    (Document(..), Parser, parseCategory, parseLocale)
import           Types

-- | Parse description
parseQDesc' :: LT.Text -> Parser (LT.Text, LT.Text, LT.Text)
parseQDesc' txt = case LT.lines txt of
                      (_:question:_:locale:_:solution) -> return
                          (question, locale, LT.unlines solution)
                      _                                -> Left InvalidFormat

-- | Parse description
parseQDesc :: LT.Text -> Parser FAQDescription
parseQDesc txt = do
    (question, locale, solution) <- parseQDesc' txt
    parsedLocale <- parseLocale locale
    return $ QDescription parsedLocale question solution

-- | Parse FAQ directory
parseFAQ :: Document -> Parser FAQ
parseFAQ Document{..} = do
    descriptions <- mapM parseQDesc docDescription
    category <- parseFAQMetaDatas $ LT.lines docMetadata
    return       $ FAQ category descriptions

-- | Parse metadata
parseFAQMetaDatas :: [LT.Text] -> Parser Category
parseFAQMetaDatas (cat:_) = parseCategory cat
parseFAQMetaDatas _       = Left InvalidFormat
