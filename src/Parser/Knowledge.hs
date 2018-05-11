{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parser.Knowledge
    ( parseKnowledge
    ) where

import           RIO

import qualified RIO.Text as T

import           Exceptions
import           Parser.Util    (Document (..), Parser, parseCategory,
                                 parseLocale, parseMetadata)
import           Types          (Category, KDescription (..), Knowledge (..))

-- | Parse description
parseKDesc' :: Text -> Parser (Text, Text, Text)
parseKDesc' txt = case T.lines txt of
                      (_:problem:_:locale:_:solution) ->
                          return (problem, locale, T.unlines solution)
                      _                               -> Left InvalidFormat
-- | Parse description
parseKDesc :: Text -> Parser KDescription
parseKDesc txt = do
    (problem, locale, solution) <- parseKDesc' txt
    parsedLocale <- parseLocale locale
    return $ KDescription parsedLocale problem solution

-- | Parse knowledge's meta data
parseKnowledgeMeta :: Text -> Parser (Text, Category, Text)
parseKnowledgeMeta txt = case T.lines txt of
                             (errCode:ecat:etxt:_) -> do
                                  errorCategory <- parseCategory ecat
                                  errorCode     <- parseMetadata "errorcode" errCode
                                  errorText     <- parseMetadata "errortext" etxt
                                  return (errorCode, errorCategory, errorText)
                             _               -> Left InvalidFormat

-- | Parse knowledge directory
parseKnowledge :: Document -> Parser Knowledge
parseKnowledge Document{..} = do
    descriptions <- mapM parseKDesc docDescription
    (eCode, eCategory, eText) <- parseKnowledgeMeta docMetadata
    return $ Knowledge eCode eCategory eText descriptions
