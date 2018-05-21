{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards  #-}

module Parser.FAQ
     ( parseFAQ
     ) where

import           RIO

import           Data.Extensible
import qualified RIO.Text as T

import           Exceptions
import           Parser.Util (Document, Parser, parseCategory, parseLocale)
import           Types

-- | Parse description
parseQDesc' :: Text -> Parser (Text, Text, Text)
parseQDesc' txt = case T.lines txt of
    (_:question:_:locale:_:solution) -> return
        (question, locale, T.unlines solution)
    _                                -> Left InvalidFormat

-- | Parse description
parseQDesc :: Text -> Parser FAQDescription
parseQDesc txt = do
    (question, locale, solution) <- parseQDesc' txt
    parsedLocale <- parseLocale locale
    return $ #locale   @= parsedLocale
          <: #question @= question
          <: #solution @= solution
          <: nil

-- | Parse FAQ directory
parseFAQ :: Document -> Parser FAQ
parseFAQ doc = do
    descriptions <- mapM parseQDesc (doc ^. #descriptions)
    category <- parseFAQMetaDatas $ T.lines (doc ^. #metadata)
    return $ #category     @= category
          <: #descriptions @= descriptions
          <: nil

-- | Parse metadata
parseFAQMetaDatas :: [Text] -> Parser Category
parseFAQMetaDatas (cat:_) = parseCategory cat
parseFAQMetaDatas _       = Left InvalidFormat
